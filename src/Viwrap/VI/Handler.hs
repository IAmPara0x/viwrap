module Viwrap.VI.Handler
  ( autoCompleteP
  , handleNewline
  , handleTab
  , handleVIHook
  , handleVITerminal
  ) where

import Control.Monad              (when)
import Control.Monad.Freer        (Eff, Members, interpret)
import Control.Monad.Freer.Reader (Reader, asks)
import Control.Monad.Freer.State  (State, get, modify)

import Lens.Micro                 ((%~), (.~), (?~), (^.))
import Text.Printf (printf)
import Viwrap.Pty
import Viwrap.Pty.Utils           (writeMaster, writeStdout)
import Viwrap.VI

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe                 (fromMaybe)
import Data.String                (fromString)
import Data.Void                  (Void)
import Text.Megaparsec            (Parsec, choice, eof, optional, some, runParser)
import Text.Megaparsec.Byte       (printChar, tab)
import Viwrap.ANSI                (ansiParser)

import System.Console.ANSI        qualified as ANSI


type ViwrapEff' fd effs
  = Members '[HandleAct fd , Logger , Process , Reader (Env fd) , State ViwrapState] effs

handleVITerminal
  :: forall fd a effs . (ViwrapEff' fd effs) => Eff (VIEdit ': effs) a -> Eff effs a
handleVITerminal = interpret \case
  Backspace    -> backspaceTerminal @fd
  MoveLeft  n  -> moveLeftTerminal @fd n >> _viLine <$> get
  MoveRight n  -> moveRightTerminal @fd n >> _viLine <$> get
  InsertBS  bs -> insertCharTerminal @fd bs


insertCharTerminal :: forall fd effs . (ViwrapEff' fd effs) => ByteString -> Eff effs VILine
insertCharTerminal inputBS = do
  VILine {..} <- _viLine <$> get

  let (startContent, endContent) = BS.splitAt _viCursorPos _viLineContent
      newLineContent             = mconcat [startContent, inputBS, endContent]
      movePos                    = BS.length _viLineContent - _viCursorPos

  hmaster <- asks @(Env fd) (snd . (^. masterPty))

  eraseAndWrite @fd hmaster (BS.length endContent) $ mconcat [inputBS, endContent]

  writeStdout @fd
    $ mconcat [fromString $ ANSI.cursorForwardCode movePos, fromString ANSI.hideCursorCode]

  modify (viLine %~ updateVILine newLineContent)
  modify (viHook ?~ SyncCursor)

  r <- _viLine <$> get
  logM "insertCharTerminal" [show r]
  return r

moveLeftTerminal :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveLeftTerminal n = do

  VILine {..} <- _viLine <$> get

  let movePos = min n _viCursorPos

  modify (viLine . viCursorPos %~ (\x -> x - movePos))
  writeStdout @fd (fromString $ ANSI.cursorBackwardCode movePos)

  r <- _viLine <$> get
  logM "moveLeftTerminal" [show r]

moveRightTerminal :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveRightTerminal n = do
  VILine {..} <- _viLine <$> get

  let movePos = min n (BS.length _viLineContent - _viCursorPos)

  modify (viLine . viCursorPos %~ (+ movePos))
  writeStdout @fd (fromString $ ANSI.cursorForwardCode movePos)
  r <- _viLine <$> get
  logM "moveRightTerminal" [show r]

backspaceTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs VILine
backspaceTerminal = do

  VILine {..} <- _viLine <$> get


  hmaster     <- asks @(Env fd) (snd . (^. masterPty))

  let
    finalCursorPos = _viCursorPos - 1
    startContent   = BS.take finalCursorPos _viLineContent
    endContent     = BS.drop (finalCursorPos + 1) _viLineContent
    newLineContent = startContent <> endContent
    endContentLen  = BS.length endContent
    movePos        = BS.length _viLineContent - _viCursorPos

    modifyVILine   = do
      eraseAndWrite @fd hmaster (endContentLen + 1) endContent
      writeStdout @fd
        $ mconcat [fromString $ ANSI.cursorForwardCode movePos, fromString ANSI.hideCursorCode]
      modify (viHook ?~ SyncCursor)
      modify (viLine %~ updateVILine newLineContent)

  when (0 < _viCursorPos) modifyVILine
  r <- _viLine <$> get
  logM "backspaceTerminal" [show r]
  return r

handleNewline :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleNewline = do
  writeMaster @fd "\n"
  modify (viLine .~ initialVILine)
  modify (isPromptUp .~ False)
  modify (viHook ?~ SyncCursor)


handleTab :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleTab = do
  VILine {..} <- _viLine <$> get

  when
    (BS.length _viLineContent == _viCursorPos)
    do

      writeMaster @fd (BS.singleton 9)
      modify (viHook ?~ TabPressed)
      r <- _viLine <$> get
      logM "handleTab" [show r]

handleVIHook :: forall fd effs . (ViwrapEff fd effs) => VIHook -> Eff effs ()
handleVIHook SyncCursor = do

  ViwrapState { _viLine = VILine {..}, _prevMasterContent } <- get

  when
    (_prevMasterContent == mempty)
    do
      modify (viHook .~ Nothing)

      writeStdout @fd $ foldMap
        fromString
        [ANSI.cursorBackwardCode $ BS.length _viLineContent - _viCursorPos, ANSI.showCursorCode]

handleVIHook TabPressed = do
  ViwrapState {_prevMasterContent, _viLine=VILine{..}} <- get

  let result = either (error . show) id (runParser autoCompleteP "" _prevMasterContent)

  logM "handleVIHook" ["TabPressed", show $ runParser autoCompleteP "" _prevMasterContent]
  modify (viLine %~ updateVILine (_viLineContent <> result))
  modify (viHook .~ Nothing)

  r <- _viLine <$> get
  logM "handleVIHook" ["TabPressed", printf "VILine: %s" $ show r]


type Parser = Parsec Void ByteString

autoCompleteP :: Parser ByteString
autoCompleteP = do
  inputEnded <- optional eof

  let dropNonPrintableChar = do
        ansiseq <- ansiParser
        if ansiseq == "\n" then return mempty else autoCompleteP

  case inputEnded of
    Nothing -> do
      mresult <- optional $ choice [some printChar, (: []) <$> tab]
      maybe dropNonPrintableChar (return . foldMap BS.singleton) mresult
    _ -> return mempty


eraseAndWrite
  :: forall fd effs
   . (Members '[HandleAct fd] effs)
  => ToHandle fd
  -> Int
  -> ByteString
  -> Eff effs ()
eraseAndWrite h n content = hWrite @fd h $ mconcat [BS.replicate n 8, content]

