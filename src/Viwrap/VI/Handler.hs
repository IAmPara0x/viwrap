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
import Data.Sequence              qualified as Seq
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
  ( Cursor (..)
  , addHook
  , autoCompleteP
  , eraseAndWrite
  , moveCursor
  , removeHook
  , timeoutAndRemove
  )
import Viwrap.Zipper

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

  VIState {..} <- _viState <$> get

  logVI ["insertCharTerminal"]
    $ printf "inserting %s when zipper state is %s" (show inputBS) (show _currentLine)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  modify (viState . currentLine %~ insertZipper inputBS)

  let movePos = BS.length $ _zipperFocus _currentLine

  eraseAndWrite hmaster movePos $ mconcat [inputBS, _zipperFocus _currentLine]

  writeStdout $ fromString ANSI.hideCursorCode
  moveCursor (Forward movePos)

  modify (currentPollRate .~ div pollingRate 2)
  addHook SyncCursor


  r <- _viState <$> get
  logVI ["insertCharTerminal"] $ show r

moveLeftTerminal :: (ViwrapEff' effs) => Int -> Eff effs ()
moveLeftTerminal n = do

  VIState {..} <- _viState <$> get

  let movePos = min n $ BS.length (_zipperCrumbs _currentLine)

  modify (viState . currentLine %~ backwardZipper n)

  moveCursor (Backward movePos)

  r <- _viState <$> get
  logVI ["moveLeftTerminal"] $ show r

moveRightTerminal :: (ViwrapEff' effs) => Int -> Eff effs ()
moveRightTerminal n = do

  VIState {..} <- _viState <$> get

  let movePos = min n $ BS.length (_zipperFocus _currentLine)
  modify (viState . currentLine %~ forwardZipper n)

  moveCursor (Forward movePos)
  r <- _viState <$> get
  logVI ["moveRightTerminal"] $ show r

backspaceTerminal :: (ViwrapEff' effs) => Eff effs ()
backspaceTerminal = do

  VIState {..} <- _viState <$> get
  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  modify (viState . currentLine %~ deleteZipper)

  let movePos = BS.length $ _zipperFocus _currentLine

  eraseAndWrite hmaster (movePos + 1) (_zipperFocus _currentLine)
  writeStdout (fromString ANSI.hideCursorCode)

  moveCursor (Forward movePos)

  addHook SyncCursor
  modify (currentPollRate .~ div pollingRate 2)

  r <- _viState <$> get
  logVI ["backspaceTerminal"] $ show r

handleNewline :: (ViwrapEff effs) => Eff effs ()
handleNewline = do

  VIState{..} <- _viState <$> get

  writeMaster "\n"

  logVI ["handleNewline"] "Received '\\n' setting the VI line to initial state"

  when (_currentLine /= mempty)
    $ modify (viState . prevLines %~ insertZipper (Seq.singleton _currentLine))
  modify (viState . currentLine .~ mempty)
  modify (isPromptUp .~ False)
  addHook SyncCursor

handleTab :: (ViwrapEff effs) => Eff effs ()
handleTab = do

  VIState {..} <- _viState <$> get

  logVI ["handleTab"] $ printf "Received '\\t' at %s" (show _currentLine)
  when
    (_zipperFocus _currentLine == mempty)
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


  ViwrapState { _prevMasterContent, _viState = VIState { _currentLine = Zipper {..} } } <- get


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

  ViwrapState { _prevMasterContent } <- get

  let result = either (error . show) id (runParser autoCompleteP "" _prevMasterContent)
  logVI ["handleVIHook", "TabPressed"] (show result)

  case result of
    Nothing                     -> return ()
    (Just CompletionList      ) -> removeHook
    (Just (Completion content)) -> do
      removeHook
      modify (viState . currentLine %~ insertZipper content)
