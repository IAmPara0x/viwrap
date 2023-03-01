module Viwrap.VI.Handler
  ( handleNewline
  , handleTab
  , handleVIHook
  , handleVITerminal
  ) where

import Control.Monad              (when)
import Control.Monad.Freer        (Eff, Members, interpret)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State  (State, get, modify)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Sequence              (Seq (..))

import Data.String                (fromString)

import Lens.Micro                 ((%~), (.~))
import System.Console.ANSI        qualified as ANSI

import Text.Megaparsec            (runParser)
import Text.Printf                (printf)


import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Utils           (writeMaster, writeStdout)
import Viwrap.VI
import Viwrap.VI.Utils
  ( addHook
  , autoCompleteP
  , eraseAndWrite
  , removeHook
  , timeoutAndRemove
  )

type ViwrapEff' fd effs
  = Members '[HandleAct fd , Logger , Process , Reader (Env fd) , State ViwrapState] effs

handleVITerminal
  :: forall fd a effs . (ViwrapEff' fd effs) => Eff (VIEdit ': effs) a -> Eff effs a
handleVITerminal = interpret \case
  Backspace    -> backspaceTerminal @fd
  MoveLeft  n  -> moveLeftTerminal @fd n
  MoveRight n  -> moveRightTerminal @fd n
  InsertBS  bs -> insertCharTerminal @fd bs


insertCharTerminal :: forall fd effs . (ViwrapEff' fd effs) => ByteString -> Eff effs ()
insertCharTerminal inputBS = do
  VILine { _viLineContent = z@Zipper {..} } <- _viLine <$> get

  logVI ["insertCharTerminal"]
    $ printf "inserting %s when zipper state is %s" (show inputBS) (show z)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask @(Env fd)

  modify (viLine . viLineContent %~ insertZipper inputBS)

  eraseAndWrite @fd hmaster (BS.length _zipperFocus) $ mconcat [inputBS, _zipperFocus]

  writeStdout @fd $ mconcat
    [fromString $ ANSI.cursorForwardCode (BS.length _zipperFocus), fromString ANSI.hideCursorCode]

  modify (currentPollRate .~ div pollingRate 2)
  addHook SyncCursor

  r <- _viLine <$> get
  logVI ["insertCharTerminal"] $ show r

moveLeftTerminal :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveLeftTerminal n = do

  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ backwardZipper n)

  let movePos = min n (BS.length _zipperCrumbs)

  logVI ["moveLeftTerminal"] $ printf "moving cursor to left by %d" movePos

  writeStdout @fd (fromString $ ANSI.cursorBackwardCode movePos)

  r <- _viLine <$> get
  logVI ["moveLeftTerminal"] $ show r

moveRightTerminal :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveRightTerminal n = do

  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ forwardZipper n)

  let movePos = min n (BS.length _zipperFocus)

  logVI ["moveRightTerminal"] $ printf "moving cursor to right by %d" movePos

  writeStdout @fd (fromString $ ANSI.cursorForwardCode movePos)
  r <- _viLine <$> get
  logVI ["moveRightTerminal"] $ show r

backspaceTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs ()
backspaceTerminal = do


  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ deleteZipper)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask @(Env fd)

  eraseAndWrite @fd hmaster (BS.length _zipperFocus + 1) _zipperFocus

  writeStdout @fd $ mconcat
    [fromString $ ANSI.cursorForwardCode (BS.length _zipperFocus), fromString ANSI.hideCursorCode]

  addHook SyncCursor
  modify (currentPollRate .~ div pollingRate 2)

  r <- _viLine <$> get
  logVI ["backspaceTerminal"] $ show r

handleNewline :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleNewline = do
  writeMaster @fd "\n"
  logVI ["handleNewline"] "Received '\\n' setting the VI line to initial state"
  modify (viLine .~ initialVILine)
  modify (isPromptUp .~ False)
  addHook SyncCursor

handleTab :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleTab = do
  line@VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  logVI ["handleTab"] $ printf "Received '\\t' at %s" (show line)
  when
    (_zipperFocus == mempty)
    do
      writeMaster @fd (BS.singleton 9)
      addHook TabPressed

handleVIHook :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleVIHook = do
  hooks <- _viHooks <$> get

  case hooks of
    Empty              -> return ()
    (SyncCursor :<| _) -> handleSyncCursor @fd
    (TabPressed :<| _) -> handleTabPressed @fd

handleSyncCursor :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleSyncCursor = do
  ViwrapState { _viLine = VILine { _viLineContent = Zipper {..} }, _prevMasterContent } <- get

  Env { _envPollingRate } <- ask @(Env fd)

  logVI ["SyncCursor"] $ printf "PrevMasterContent: %s" (show _prevMasterContent)

  when
    (_prevMasterContent == mempty)
    do

      removeHook

      let movePos = BS.length _zipperFocus

      logVI ["SyncCursor"] $ printf "Moving cursor to left by: %d" movePos

      modify (currentPollRate .~ _envPollingRate)

      writeStdout @fd $ foldMap fromString [ANSI.cursorBackwardCode movePos, ANSI.showCursorCode]


handleTabPressed :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleTabPressed = timeoutAndRemove @fd do

  ViwrapState { _prevMasterContent, _viLine = VILine { _viLineContent } } <- get

  let result = either (error . show) id (runParser autoCompleteP "" _prevMasterContent)
  logVI ["handleVIHook", "TabPressed"] (show result)

  case result of
    Nothing                     -> return ()
    (Just CompletionList      ) -> removeHook
    (Just (Completion content)) -> do
      removeHook
      modify (viLine . viLineContent %~ insertZipper content)
