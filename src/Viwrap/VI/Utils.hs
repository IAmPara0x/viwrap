module Viwrap.VI.Utils
  ( addHook
  , autoCompleteP
  , eraseAndWrite
  , moveToBeginning
  , moveToEnd
  , moveToPrevLine
  , moveToNextLine
  , Cursor (..)
  , moveCursor
  , removeHook
  , timeoutAndRemove
  , toInsertMode
  , toNormalMode
  ) where

import Control.Monad              (void)
import Control.Monad.Freer        (Eff, Members)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State  (State, get, modify)

import Lens.Micro                 ((%~), (.~), (^.))

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe                 (fromJust)
import Data.Sequence              ((|>), Seq(..))
import Data.Sequence              qualified as Seq
import Data.String                (fromString)
import Data.Void                  (Void)
import System.Console.ANSI        qualified as ANSI
import System.IO                  (Handle)

import Text.Megaparsec            (Parsec, choice, eof, optional, some)
import Text.Megaparsec.Byte       (printChar, tab)
import Text.Printf                (printf)
import Viwrap.ANSI                (ansiParser)
import Viwrap.Logger              (Logger, logVI)
import Viwrap.Pty
import Viwrap.Pty.Utils           (writeStdout)
import Viwrap.VI
import Viwrap.Zipper

moveToBeginning :: ViwrapEff effs => Eff effs ()
moveToBeginning = do
  VIState {..} <- _viState <$> get
  void $ moveLeft (BS.length $ _currentLine ^. zipperCrumbs)

moveToEnd :: ViwrapEff effs => Eff effs ()
moveToEnd = do
  VIState {..} <- _viState <$> get
  void $ moveRight (BS.length $ _currentLine ^. zipperFocus)

moveToPrevLine :: ViwrapEff effs => Eff effs ()
moveToPrevLine = do

  s@VIState{..} <-  _viState <$> get

  logVI ["moveToPrevLine"] $ printf "vistate: %s" (show s)

  case _zipperCrumbs _prevLines of
    Empty -> return ()
    prevLine :<| _ -> do
      Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

      let currentContent = contentZipper _currentLine
          prevContent = contentZipper prevLine

      eraseAndWrite hmaster (BS.length currentContent)  prevContent

      writeStdout $ fromString ANSI.hideCursorCode
      moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      modify (viState . prevLines %~ backwardZipper 1)
      modify (viState . currentLine .~ prevLine)
      modify (currentPollRate .~ div pollingRate 2)

      addHook SyncCursor

moveToNextLine :: ViwrapEff effs => Eff effs ()
moveToNextLine = do

  modify (viState . prevLines %~ forwardZipper 1)

  s@VIState{..} <-  _viState <$> get

  logVI ["moveToNextLine"] $ printf "vistate: %s" (show s)

  case _zipperFocus _prevLines of
    Empty -> return ()
    nextLine :<| _ -> do
      Env { _masterPty = (_, hmaster), _envPollingRate = pollingRate } <- ask

      let currentContent = contentZipper _currentLine
          nextContent = contentZipper nextLine

      eraseAndWrite hmaster (BS.length currentContent) nextContent

      writeStdout $ fromString ANSI.hideCursorCode
      moveCursor (Forward $ BS.length $ _zipperFocus _currentLine)

      modify (viState . currentLine .~ nextLine)
      modify (currentPollRate .~ div pollingRate 2)

      addHook SyncCursor

toInsertMode :: ViwrapEff effs => Eff effs ()
toInsertMode = do
  toMode Insert
  VIState {_prevLines} <- _viState <$> get
  modify (viState . prevLines %~ forwardZipper (Seq.length $ _zipperFocus _prevLines))

toNormalMode :: ViwrapEff effs => Eff effs ()
toNormalMode = do
  toMode Normal
  moveLeft 1

toMode :: ViwrapEff effs => VIMode -> Eff effs ()
toMode mode = modify (viState . viMode .~ mode)

type Parser = Parsec Void ByteString

autoCompleteP :: Parser (Maybe AutoComplete)
autoCompleteP = do
  inputEnded <- optional eof

  let dropNonPrintableChar = do
        ansiseq <- ansiParser
        if ansiseq == "\n" then return (Just CompletionList) else autoCompleteP

  case inputEnded of
    Nothing -> do
      mresult <- optional $ choice [some printChar, (: []) <$> tab]
      maybe dropNonPrintableChar (return . Just . Completion . foldMap BS.singleton) mresult
    _ -> return Nothing

eraseAndWrite :: (Members '[HandleAct] effs) => Handle -> Int -> ByteString -> Eff effs ()
eraseAndWrite h n content = hWrite h $ mconcat [BS.replicate n 8, content]

addHook :: (Members '[State ViwrapState , Logger] effs) => VIHook -> Eff effs ()
addHook hook = do
  hooks <- _viHooks <$> get
  logVI ["addHook"] $ printf "Adding %s to %s" (show hook) (show hooks)
  modify (viHooks %~ (|> hook))

removeHook :: (Members '[State ViwrapState] effs) => Eff effs ()
removeHook = modify (viHooks %~ Seq.drop 1)

timeoutAndRemove :: (ViwrapEff effs) => Eff effs () -> Eff effs ()
timeoutAndRemove runHook = do
  content <- _prevMasterContent <$> get
  if content == mempty then removeHook else runHook


data Cursor
  = Forward Int
  | Backward Int
  deriving stock (Eq, Show)

moveCursor :: Members '[HandleAct , Logger , Reader Env] effs => Cursor -> Eff effs ()
moveCursor cursor = do
  stdout    <- snd <$> getStdout
  stdin     <- snd <$> getStdout
  cursorPos <- hCursorPos stdout
  termsize  <- hTerminalSize stdin

  logVI ["moveCursor"] $ printf "cursor pos: %s, termsize: %s" (show cursorPos) (show termsize)

  -- TODO: find a better way to solve this.
  let movePos = case cursor of
        Forward  n -> n
        Backward n -> n

      (_       , width    ) = fromJust termsize
      (row     , col      ) = fromJust cursorPos
      (vertMove, horizMove) = (movePos `div` width, movePos `mod` width)
      (nrow    , ncol     ) = case cursor of

        (Forward _)
          | col + horizMove >= width -> (row + vertMove + 1, (col + horizMove) `mod` width)
          | otherwise                -> (row + vertMove, col + horizMove)

        (Backward _) | col >= horizMove -> (row - vertMove, col - horizMove)
                     | otherwise        -> (row - vertMove - 1, col + width - horizMove)

  writeStdout (fromString $ ANSI.setCursorPositionCode nrow ncol)
