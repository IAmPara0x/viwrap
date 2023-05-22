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
  , moveToNextWord
  , moveToPrevWord
  ) where

import Control.Monad              (unless, void, when)
import Control.Monad.Freer        (Eff, raise)
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.State  (get, modify)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.ByteString.Internal   qualified as BS
import Data.Char                  qualified as Char
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.String                (fromString)
import Data.Word (Word8)

import Lens.Micro                 ((%~), (.~), (^.))

import System.Console.ANSI        qualified as ANSI

import Text.Megaparsec            (runParser)
import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Utils           (getMasterPty, writeMaster, writeStdout)
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

viBracket :: forall a b . Eff ViwrapStack a -> Eff ViStack b -> Eff ViwrapStack b
viBracket ioEff modifyViStateEff = do
  void ioEff
  raise $ raise modifyViStateEff

insertBS :: ByteString -> Eff ViwrapStack ()
insertBS inputBS = do

  let
    logLabels = ["insertBS"]

    ioEff :: Eff ViwrapStack ()
    ioEff = do
      hmaster      <- getMasterPty
      VIState {..} <- _viState <$> get
      let movePos = BS.length $ _zipperFocus _currentLine

      logVI logLabels
        $ printf "inserting %s when zipper state is %s" (show inputBS) (show _currentLine)
      eraseAndWrite hmaster movePos $ mconcat [inputBS, _zipperFocus _currentLine]

      writeStdout $ fromString ANSI.hideCursorCode
      moveCursor (Forward movePos)

  viBracket
    ioEff
    do
      Env { _envPollingRate = pollingRate } <- ask

      modify (viState . currentLine %~ insertZipper inputBS)

      modify (currentPollRate .~ div pollingRate 2)
      addHook SyncCursor

      logVI logLabels . show . (^. currentLine) . _viState =<< get


insertNoUpdate :: ByteString -> Eff ViwrapStack ()
insertNoUpdate inputBS = do

  let logLabels = ["insertNoUpdate"]

      ioEff :: Eff ViwrapStack ()
      ioEff = do
        hmaster      <- getMasterPty
        VIState {..} <- _viState <$> get
        let movePos = BS.length $ _zipperFocus _currentLine

        logVI logLabels $ printf "input: %s" (show inputBS)

        eraseAndWrite hmaster movePos $ mconcat [inputBS, _zipperFocus _currentLine]
        writeStdout $ fromString ANSI.hideCursorCode
        moveCursor (Forward movePos)

  viBracket
    ioEff
    do
      Env { _envPollingRate = pollingRate } <- ask

      modify (currentPollRate .~ div pollingRate 2)
      addHook SyncCursor

      logVI logLabels . show . (^. currentLine) . _viState =<< get

moveLeft :: Int -> Eff ViwrapStack ()
moveLeft n = do

  let logLabels = ["moveLeft"]

      ioEff :: Eff ViwrapStack ()
      ioEff = do
        VIState {..} <- _viState <$> get
        let movePos = min n $ BS.length (_zipperCrumbs _currentLine)
        moveCursor (Backward movePos)

  viBracket
    ioEff
    do
      modify (viState . currentLine %~ backwardZipper n)
      logVI logLabels . show . (^. currentLine) . _viState =<< get

moveRight :: Int -> Eff ViwrapStack ()
moveRight n = do

  let logLabels = ["moveRight"]

      ioEff :: Eff ViwrapStack ()
      ioEff = do

        VIState {..} <- _viState <$> get
        let movePos = min n $ BS.length (_zipperFocus _currentLine)
        moveCursor (Forward movePos)

  viBracket
    ioEff
    do
      modify (viState . currentLine %~ forwardZipper n)
      logVI logLabels . show . (^. currentLine) . _viState =<< get

backspace :: Int -> Eff ViwrapStack ()
backspace n = do

  VIState {..} <- _viState <$> get

  let logLabels   = ["backspace"]
      emptyCrumbs = BS.null $ _zipperCrumbs _currentLine
      movePos     = BS.length $ _zipperFocus _currentLine

      ioEff :: Eff ViwrapStack ()
      ioEff = unless
        emptyCrumbs
        do
          hmaster <- getMasterPty
          eraseAndWrite hmaster (movePos + n) (_zipperFocus _currentLine)
          writeStdout (fromString ANSI.hideCursorCode)
          moveCursor (Forward movePos)

  viBracket ioEff $ unless
    emptyCrumbs
    do

      Env { _envPollingRate = pollingRate } <- ask

      modify (viState . currentLine %~ deleteZipper n)
      addHook SyncCursor
      modify (currentPollRate .~ div pollingRate 2)
      logVI logLabels . show . (^. currentLine) . _viState =<< get

moveToBeginning :: Eff ViwrapStack ()
moveToBeginning = do
  VIState {..} <- _viState <$> get
  moveLeft (BS.length $ _currentLine ^. zipperCrumbs)

moveToEnd :: Eff ViwrapStack ()
moveToEnd = do
  VIState {..} <- _viState <$> get
  moveRight (BS.length $ _currentLine ^. zipperFocus)

handleNewline :: Eff ViwrapStack ()
handleNewline = do

  let ioEff :: Eff ViwrapStack ()
      ioEff = writeMaster "\n"

  viBracket
    ioEff
    do

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
          modify (viState . prevLines %~ deleteZipper 1)

      logVI ["handleNewline"] "Received '\\n' setting the VI line to initial state"

      when (_currentLine /= mempty)
        $ modify (viState . prevLines %~ insertZipper (Seq.singleton _currentLine))

      modify (viState . currentLine .~ mempty)
      modify (viState . viMode .~ Insert)
      modify (isPromptUp .~ False)
      addHook SyncCursor

handleTab :: Eff ViwrapStack ()
handleTab = do

  let ioEff :: Eff ViwrapStack ()
      ioEff = do
        writeMaster (BS.singleton 9)

  viBracket
    ioEff
    do
      VIState {..} <- _viState <$> get
      when (_zipperFocus _currentLine == mempty) $ addHook TabPressed

      logVI ["handleTab"] $ printf "Received '\\t' at %s" (show _currentLine)

moveToPrevLine :: Eff ViwrapStack ()
moveToPrevLine = do

  VIState {..} <- _viState <$> get

  let logLabels = ["moveToPrevLine"]

  case _zipperCrumbs _prevLines of
    Empty            -> pure ()
    (prevLine :<| _) -> do

      let currentContent = contentZipper _currentLine
          prevContent    = contentZipper prevLine

          ioEff :: Eff ViwrapStack ()
          ioEff = do
            hmaster <- getMasterPty
            eraseAndWrite hmaster (BS.length currentContent) prevContent
            writeStdout $ fromString ANSI.hideCursorCode
            moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      viBracket
        ioEff
        do
          Env { _envPollingRate = pollingRate } <- ask

          logVI logLabels $ printf "setting current line to: %s" (show prevLine)

          modify (viState . prevLines %~ backwardZipper 1)
          modify (viState . currentLine .~ prevLine)
          modify (currentPollRate .~ div pollingRate 2)
          addHook SyncCursor

moveToNextLine :: Eff ViwrapStack ()
moveToNextLine = do

  VIState {..} <- _viState <$> get
  --

  let logLabels   = ["moveToNextLine"]
  -- get all the lines that are in the front of the history currently, since
  -- the 1st element of the zipperFocus is always the current element we drop that element
      futureLines = Seq.drop 1 $ _zipperFocus _prevLines

  case futureLines of
    Empty            -> pure ()
    (nextLine :<| _) -> do


      let currentContent = contentZipper _currentLine
          nextContent    = contentZipper nextLine

          ioEff :: Eff ViwrapStack ()
          ioEff = do

            hmaster <- getMasterPty
            eraseAndWrite hmaster (BS.length currentContent) nextContent

            writeStdout $ fromString ANSI.hideCursorCode
            moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      viBracket
        ioEff
        do

          logVI logLabels $ printf "setting current line to: %s" (show nextLine)
          Env { _envPollingRate = pollingRate } <- ask

          modify (viState . prevLines %~ forwardZipper 1)
          modify (viState . currentLine .~ nextLine)
          modify (currentPollRate .~ div pollingRate 2)

          addHook SyncCursor

handleVIHook :: Eff ViwrapStack ()
handleVIHook = do
  hooks <- _viHooks <$> get

  case hooks of
    Empty              -> pure ()
    (SyncCursor :<| _) -> handleSyncCursor
    (TabPressed :<| _) -> handleTabPressed

handleSyncCursor :: Eff ViwrapStack ()
handleSyncCursor = do

  ViwrapState { _prevMasterContent, _viState = VIState { _currentLine = Zipper {..} } } <- get

  let logLabels = ["SyncCursor"]
      noContent = _prevMasterContent == mempty

      ioEff :: Eff ViwrapStack ()
      ioEff = when
        noContent
        do
          moveCursor (Backward $ BS.length _zipperFocus)
          writeStdout (fromString ANSI.showCursorCode)

  viBracket ioEff $ when
    noContent
    do
      logVI logLabels $ printf "PrevMasterContent: %s" (show _prevMasterContent)
      Env { _envPollingRate } <- ask
      removeHook
      modify (currentPollRate .~ _envPollingRate)

handleTabPressed :: Eff ViwrapStack ()
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

-- TODO: Move this function somewhere else
nextWordPos :: ByteString -> Int
nextWordPos = fst . uncurry (dropcount (not . isAlphaNum))
                  . dropcount isAlphaNum 0
  where
    isAlphaNum :: Word8 -> Bool
    isAlphaNum = Char.isAlphaNum . BS.w2c
    
    dropcount :: (Word8 -> Bool) -> Int -> ByteString -> (Int,ByteString)
    dropcount predicate n str
      | BS.null str = (n,mempty)
      | predicate $ BS.head str = dropcount predicate (n + 1) $ BS.tail str
      | otherwise = (n,str)

moveToNextWord :: Eff ViwrapStack ()
moveToNextWord = do
  VIState{..} <- _viState <$> get
  moveRight (nextWordPos $ _zipperFocus _currentLine)

moveToPrevWord :: Eff ViwrapStack ()
moveToPrevWord = do
  VIState{..} <- _viState <$> get
  moveLeft $ nextWordPos (_zipperCrumbs _currentLine) + 1

-- deleteWord :: Eff ViwrapStack ()
-- deleteWord = do


toInsertMode :: Eff ViwrapStack ()
toInsertMode = do
  toMode Insert
  VIState { _prevLines } <- _viState <$> get

  -- Move to the end of the history
  modify (viState . prevLines %~ forwardZipper (Seq.length $ _zipperFocus _prevLines))
  --
  -- remove the current line that was added when we insert into the insert mode
  modify (viState . prevLines %~ deleteZipper 1)

toNormalMode :: Eff ViwrapStack ()
toNormalMode = do
  toMode Normal
  moveLeft 1
  VIState { _currentLine } <- _viState <$> get
  modify (viState . prevLines . zipperFocus .~ Seq.singleton _currentLine)

toMode :: VIMode -> Eff ViwrapStack ()
toMode mode = modify (viState . viMode .~ mode)
