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

import Text.Megaparsec            (runParser)
import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Utils           (writeMaster, writeStdout)
import Viwrap.VI
import Viwrap.VI.Utils
  ( Cursor (..)
  , addHook
  , autoCompleteP
  , eraseAndWrite
  , moveCursor
  , removeHook
  , timeoutAndRemove
  )
import qualified System.Console.ANSI as ANSI

type ViwrapEff' effs
  = Members '[HandleAct , Logger , Process , Reader Env , State ViwrapState] effs

handleVITerminal :: (ViwrapEff' effs) => Eff (VIEdit ': effs) a -> Eff effs a
handleVITerminal = interpret \case
  Backspace    -> backspaceTerminal
  MoveLeft  n  -> moveLeftTerminal n
  MoveRight n  -> moveRightTerminal n
  InsertBS  bs -> insertCharTerminal bs


insertCharTerminal :: (ViwrapEff' effs) => ByteString -> Eff effs ()
insertCharTerminal inputBS = do
  VILine { _viLineContent = z@Zipper {..} } <- _viLine <$> get

  logVI ["insertCharTerminal"]
    $ printf "inserting %s when zipper state is %s" (show inputBS) (show z)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  modify (viLine . viLineContent %~ insertZipper inputBS)

  eraseAndWrite hmaster (BS.length _zipperFocus) $ mconcat [inputBS, _zipperFocus]

  writeStdout $ fromString ANSI.hideCursorCode
  moveCursor (Forward $ BS.length _zipperFocus)

  modify (currentPollRate .~ div pollingRate 2)
  addHook SyncCursor


  r <- _viLine <$> get
  logVI ["insertCharTerminal"] $ show r

moveLeftTerminal :: (ViwrapEff' effs) => Int -> Eff effs ()
moveLeftTerminal n = do

  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ backwardZipper n)

  moveCursor (Backward $ min n $ BS.length _zipperCrumbs)

  r <- _viLine <$> get
  logVI ["moveLeftTerminal"] $ show r

moveRightTerminal :: (ViwrapEff' effs) => Int -> Eff effs ()
moveRightTerminal n = do

  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ forwardZipper n)
  moveCursor (Forward $ min n $ BS.length _zipperFocus)
  r <- _viLine <$> get
  logVI ["moveRightTerminal"] $ show r

backspaceTerminal :: (ViwrapEff' effs) => Eff effs ()
backspaceTerminal = do


  VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  modify (viLine . viLineContent %~ deleteZipper)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  eraseAndWrite hmaster (BS.length _zipperFocus + 1) _zipperFocus
  writeStdout (fromString ANSI.hideCursorCode)

  moveCursor (Forward $ BS.length _zipperFocus)

  addHook SyncCursor
  modify (currentPollRate .~ div pollingRate 2)

  r <- _viLine <$> get
  logVI ["backspaceTerminal"] $ show r

handleNewline :: (ViwrapEff effs) => Eff effs ()
handleNewline = do
  writeMaster "\n"
  logVI ["handleNewline"] "Received '\\n' setting the VI line to initial state"
  modify (viLine .~ initialVILine)
  modify (isPromptUp .~ False)
  addHook SyncCursor

handleTab :: (ViwrapEff effs) => Eff effs ()
handleTab = do

  line@VILine { _viLineContent = Zipper {..} } <- _viLine <$> get

  logVI ["handleTab"] $ printf "Received '\\t' at %s" (show line)
  when
    (_zipperFocus == mempty)
    do
      writeMaster (BS.singleton 9)
      addHook TabPressed

handleVIHook :: (ViwrapEff effs) => Eff effs ()
handleVIHook = do
  hooks <- _viHooks <$> get

  case hooks of
    Empty              -> return ()
    (SyncCursor :<| _) -> handleSyncCursor
    (TabPressed :<| _) -> handleTabPressed

handleSyncCursor :: (ViwrapEff effs) => Eff effs ()
handleSyncCursor = do


  ViwrapState { _viLine = VILine { _viLineContent = Zipper {..} }, _prevMasterContent } <- get


  logVI ["SyncCursor"] $ printf "PrevMasterContent: %s" (show _prevMasterContent)

  when
    (_prevMasterContent == mempty)
    do

      Env { _envPollingRate } <- ask

      removeHook

      moveCursor (Backward $ BS.length _zipperFocus)
      writeStdout (fromString ANSI.showCursorCode)

      modify (currentPollRate .~ _envPollingRate)



handleTabPressed :: (ViwrapEff effs) => Eff effs ()
handleTabPressed = timeoutAndRemove do

  ViwrapState { _prevMasterContent, _viLine = VILine { _viLineContent } } <- get

  let result = either (error . show) id (runParser autoCompleteP "" _prevMasterContent)
  logVI ["handleVIHook", "TabPressed"] (show result)

  case result of
    Nothing                     -> return ()
    (Just CompletionList      ) -> removeHook
    (Just (Completion content)) -> do
      removeHook
      modify (viLine . viLineContent %~ insertZipper content)
