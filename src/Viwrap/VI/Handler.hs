module Viwrap.VI.Handler
  ( backspace
  , handleNewline
  , handleTab
  , handleVIHook
  , insertBS
  , insertNoUpdate
  , moveLeft
  , moveRight
  , moveToBeginning
  , moveToEnd
  , moveToNextLine
  , moveToPrevLine
  , toInsertMode
  , toNormalMode
  ) where

import Control.Monad              (unless, when)
import Control.Monad.Freer        (Eff)
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.State  (get, modify)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.String                (fromString)

import Lens.Micro                 ((%~), (.~), (^.))

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


insertBS :: (ViwrapEff effs) => ByteString -> Eff effs ()
insertBS inputBS = do

  let logLabels = ["insertBS"]

  VIState {..} <- _viState <$> get

  logVI logLabels $ printf "inserting %s when zipper state is %s" (show inputBS) (show _currentLine)

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  modify (viState . currentLine %~ insertZipper inputBS)

  let movePos = BS.length $ _zipperFocus _currentLine

  eraseAndWrite hmaster movePos $ mconcat [inputBS, _zipperFocus _currentLine]

  writeStdout $ fromString ANSI.hideCursorCode
  moveCursor (Forward movePos)

  modify (currentPollRate .~ div pollingRate 2)
  addHook SyncCursor

  logVI logLabels . show . (^. currentLine) . _viState =<< get

insertNoUpdate :: (ViwrapEff effs) => ByteString -> Eff effs ()
insertNoUpdate inputBS = do

  let logLabels = ["insertNoUpdate"]

  VIState {..} <- _viState <$> get

  Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

  logVI logLabels $ printf "input: %s" (show inputBS)

  let movePos = BS.length $ _zipperFocus _currentLine

  eraseAndWrite hmaster movePos $ mconcat [inputBS, _zipperFocus _currentLine]

  writeStdout $ fromString ANSI.hideCursorCode
  moveCursor (Forward movePos)

  modify (currentPollRate .~ div pollingRate 2)
  addHook SyncCursor

  logVI logLabels . show . (^. currentLine) . _viState =<< get

moveLeft :: (ViwrapEff effs) => Int -> Eff effs ()
moveLeft n = do

  VIState {..} <- _viState <$> get

  let movePos   = min n $ BS.length (_zipperCrumbs _currentLine)
      logLabels = ["moveLeft"]

  modify (viState . currentLine %~ backwardZipper n)

  moveCursor (Backward movePos)

  logVI logLabels . show . (^. currentLine) . _viState =<< get

moveRight :: (ViwrapEff effs) => Int -> Eff effs ()
moveRight n = do

  VIState {..} <- _viState <$> get

  let movePos   = min n $ BS.length (_zipperFocus _currentLine)
      logLabels = ["moveRight"]

  modify (viState . currentLine %~ forwardZipper n)

  moveCursor (Forward movePos)
  logVI logLabels . show . (^. currentLine) . _viState =<< get

backspace :: (ViwrapEff effs) => Eff effs ()
backspace = do

  VIState {..} <- _viState <$> get

  unless
    (BS.null $ _zipperCrumbs _currentLine)
    do

      Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

      modify (viState . currentLine %~ deleteZipper)

      let movePos   = BS.length $ _zipperFocus _currentLine
          logLabels = ["backspace"]

      eraseAndWrite hmaster (movePos + 1) (_zipperFocus _currentLine)
      writeStdout (fromString ANSI.hideCursorCode)

      moveCursor (Forward movePos)

      addHook SyncCursor
      modify (currentPollRate .~ div pollingRate 2)

      logVI logLabels . show . (^. currentLine) . _viState =<< get

moveToBeginning :: ViwrapEff effs => Eff effs ()
moveToBeginning = do
  VIState {..} <- _viState <$> get
  moveLeft (BS.length $ _currentLine ^. zipperCrumbs)

moveToEnd :: ViwrapEff effs => Eff effs ()
moveToEnd = do
  VIState {..} <- _viState <$> get
  moveRight (BS.length $ _currentLine ^. zipperFocus)

handleNewline :: (ViwrapEff effs) => Eff effs ()
handleNewline = do

  VIState {..} <- _viState <$> get

  when
    (not (null $ _zipperFocus _prevLines) && _viMode == Normal)
    do

    -- we have to ensure that we are at the end of our history, this is necessary
    -- because let's say the user is scrolling through history and then they
    -- again 'entered' that same line present in the history withtout going in the insert mode
    -- but our history is not at the end.
      modify (viState . prevLines %~ forwardZipper (Seq.length $ _zipperFocus _prevLines))

      -- remove the current line that was added when we insert into the insert mode
      modify (viState . prevLines %~ deleteZipper)

  writeMaster "\n"

  logVI ["handleNewline"] "Received '\\n' setting the VI line to initial state"

  when (_currentLine /= mempty)
    $ modify (viState . prevLines %~ insertZipper (Seq.singleton _currentLine))

  modify (viState . currentLine .~ mempty)
  modify (viState . viMode .~ Insert)
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

moveToPrevLine :: ViwrapEff effs => Eff effs ()
moveToPrevLine = do

  s@VIState {..} <- _viState <$> get

  logVI ["moveToPrevLine"] $ printf "vistate: %s" (show s)

  case _zipperCrumbs _prevLines of
    Empty          -> pure ()
    prevLine :<| _ -> do
      Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

      let currentContent = contentZipper _currentLine
          prevContent    = contentZipper prevLine

      eraseAndWrite hmaster (BS.length currentContent) prevContent

      writeStdout $ fromString ANSI.hideCursorCode
      moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      modify (viState . prevLines %~ backwardZipper 1)
      modify (viState . currentLine .~ prevLine)
      modify (currentPollRate .~ div pollingRate 2)

      addHook SyncCursor

moveToNextLine :: ViwrapEff effs => Eff effs ()
moveToNextLine = do

  modify (viState . prevLines %~ forwardZipper 1)

  s@VIState {..} <- _viState <$> get

  logVI ["moveToNextLine"] $ printf "vistate: %s" (show s)

  case _zipperFocus _prevLines of
    Empty          -> pure ()
    nextLine :<| _ -> do
      Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

      let currentContent = contentZipper _currentLine
          nextContent    = contentZipper nextLine

      eraseAndWrite hmaster (BS.length currentContent) nextContent

      writeStdout $ fromString ANSI.hideCursorCode
      moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      modify (viState . currentLine .~ nextLine)
      modify (currentPollRate .~ div pollingRate 2)

      addHook SyncCursor

handleVIHook :: (ViwrapEff effs) => Eff effs ()
handleVIHook = do
  hooks <- _viHooks <$> get

  case hooks of
    Empty              -> pure ()
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
    Nothing                     -> pure ()
    (Just CompletionList      ) -> removeHook
    (Just (Completion content)) -> do
      removeHook
      modify (viState . currentLine %~ insertZipper content)


toInsertMode :: ViwrapEff effs => Eff effs ()
toInsertMode = do
  toMode Insert
  VIState { _prevLines } <- _viState <$> get

  modify (viState . prevLines %~ forwardZipper (Seq.length $ _zipperFocus _prevLines))

  -- remove the current line that was added when we insert into the insert mode
  modify (viState . prevLines %~ deleteZipper)

toNormalMode :: ViwrapEff effs => Eff effs ()
toNormalMode = do
  toMode Normal
  moveLeft 1
  VIState { _currentLine } <- _viState <$> get
  modify (viState . prevLines %~ appendZipper (Seq.singleton _currentLine))

toMode :: ViwrapEff effs => VIMode -> Eff effs ()
toMode mode = modify (viState . viMode .~ mode)
