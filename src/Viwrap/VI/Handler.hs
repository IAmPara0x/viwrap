module Viwrap.VI.Handler
  ( handleVITerminal
  ) where

import Control.Monad              (when)
import Control.Monad.Freer        (Eff, Members, interpret)
import Control.Monad.Freer.Reader (Reader, asks)
import Control.Monad.Freer.State  (State, get, modify)

import Viwrap.Pty
import Viwrap.Pty.Utils
import Viwrap.VI

import Lens.Micro                 ((%~), (.~), (^.))

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.String                (fromString)

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

  let (startContent, endContent) = BS.splitAt _viCursorPos _viLineContents
      inputLen                   = BS.length inputBS
      finalCursorPos             = _viCursorPos + inputLen

      newLineContent             = mconcat [startContent, inputBS, endContent]

      modifyVILine               = do

        hmaster <- asks @(Env fd) (snd . (^. masterPty))

        eraseAndWrite @fd hmaster (BS.length endContent) $ mconcat [inputBS, endContent]

        moveRightTerminal @fd $ BS.length _viLineContents - _viCursorPos
        writeStdout @fd $ fromString ANSI.hideCursorCode

        modify (setCursorPos .~ True)

        modify (viLine . viCursorPos .~ finalCursorPos)
        modify (viLine . viLineContents .~ newLineContent)

  modifyVILine

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

  let movePos = min n (BS.length _viLineContents - _viCursorPos)

  modify (viLine . viCursorPos %~ (+ movePos))
  writeStdout @fd (fromString $ ANSI.cursorForwardCode movePos)
  r <- _viLine <$> get
  logM "moveRightTerminal" [show r]

backspaceTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs VILine
backspaceTerminal = do

  VILine {..} <- _viLine <$> get


  hmaster     <- asks @(Env fd) (snd . (^. masterPty))

  let finalCursorPos = _viCursorPos - 1
      startContent   = BS.take finalCursorPos _viLineContents
      endContent     = BS.drop (finalCursorPos + 1) _viLineContents
      newLineContent = startContent <> endContent
      endContentLen  = BS.length endContent

      modifyVILine   = do
        eraseAndWrite @fd hmaster (endContentLen + 1) endContent
        moveRightTerminal @fd $ BS.length _viLineContents - _viCursorPos
        modify (setCursorPos .~ True)

        modify (viLine . viCursorPos .~ finalCursorPos)
        modify (viLine . viLineContents .~ newLineContent)

  when (0 < _viCursorPos) modifyVILine
  r <- _viLine <$> get
  logM "backspaceTerminal" [show r]
  return r

eraseAndWrite
  :: forall fd effs . (ViwrapEff' fd effs) => ToHandle fd -> Int -> ByteString -> Eff effs ()
eraseAndWrite h n content = hWrite @fd h $ mconcat [BS.replicate n 8, content]
