module Viwrap.VI.Handler
  ( handleVITerminal
  ) where

import Control.Monad              (void, when)
import Control.Monad.Freer        (Eff, Members, interpret)
import Control.Monad.Freer.Reader (Reader, asks)
import Control.Monad.Freer.State  (State, get, modify)

import Viwrap.Pty
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
  Backspace   -> backspaceTerminal @fd
  MoveLeft    -> moveLeftTerminal @fd
  MoveRight   -> moveRightTerminal @fd
  InsertBS bs -> insertCharTerminal @fd bs


insertCharTerminal :: forall fd effs . (ViwrapEff' fd effs) => ByteString -> Eff effs VILine
insertCharTerminal inputBS = do
  VILine {..} <- _viLine <$> get

  let (startContent, endContent) = BS.splitAt _viCursorPos _viLineContents
      inputLen                   = BS.length inputBS
      finalCursorPos             = _viCursorPos + inputLen

      newLineContent             = mconcat [startContent, inputBS, endContent]

      modifyVILine               = do

        hmaster <- asks @(Env fd) (snd . (^. masterPty))
        stdout  <- snd <$> getStdout @fd

        eraseAndWrite @fd hmaster (BS.length endContent) $ mconcat [inputBS, endContent]

        moveCursorRight @fd $ BS.length _viLineContents - _viCursorPos

        eraseAndWrite @fd stdout (BS.length endContent) $ mconcat
          [ inputBS
          , endContent
          , fromString $ ANSI.cursorBackwardCode $ BS.length newLineContent - finalCursorPos
          ]

        modify (viLine . viCursorPos .~ finalCursorPos)
        modify (viLine . viLineContents .~ newLineContent)

  modifyVILine

  r <- _viLine <$> get
  logM "insertCharTerminal" [show r]
  return r

moveLeftTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs VILine
moveLeftTerminal = do
  VILine {..} <- _viLine <$> get

  let modifyVILine = do

        stdout <- snd <$> getStdout @fd
        modify (viLine . viCursorPos %~ (\x -> x - 1))
        hWrite @fd stdout (fromString $ ANSI.cursorBackwardCode 1)

  when (_viCursorPos /= 0) modifyVILine
  r <- _viLine <$> get
  logM "moveLeftTerminal" [show r]
  return r

moveRightTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs VILine
moveRightTerminal = do
  VILine {..} <- _viLine <$> get

  let modifyVILine = do
        moveCursorRight @fd 1

  when (_viCursorPos /= BS.length _viLineContents) modifyVILine
  r <- _viLine <$> get
  logM "moveRightTerminal" [show r]
  return r

backspaceTerminal :: forall fd effs . (ViwrapEff' fd effs) => Eff effs VILine
backspaceTerminal = do

  VILine {..} <- _viLine <$> get


  hmaster     <- asks @(Env fd) (snd . (^. masterPty))
  stdout      <- snd <$> getStdout @fd

  let finalCursorPos = _viCursorPos - 1
      startContent   = BS.take finalCursorPos _viLineContents
      endContent     = BS.drop (finalCursorPos + 1) _viLineContents
      newLineContent = startContent <> endContent
      endContentLen  = BS.length endContent

      modifyVILine   = do
        eraseAndWrite @fd hmaster (endContentLen + 1) endContent
        hWrite @fd stdout
          $ mconcat ["\b\ESC[K", endContent, fromString $ ANSI.cursorBackwardCode endContentLen]

        modify (viLine . viCursorPos .~ finalCursorPos)
        modify (viLine . viLineContents .~ newLineContent)

  when (0 < _viCursorPos) modifyVILine
  r <- _viLine <$> get
  logM "backspaceTerminal" [show r]
  return r


moveCursorRight :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveCursorRight n = do
  stdout <- snd <$> getStdout @fd
  hWrite @fd stdout (fromString $ ANSI.cursorForwardCode n)
  modify (viLine . viCursorPos %~ (+ n))


moveCursorLeft :: forall fd effs . (ViwrapEff' fd effs) => Int -> Eff effs ()
moveCursorLeft n = do
  stdout <- snd <$> getStdout @fd
  hWrite @fd stdout (fromString $ ANSI.cursorBackwardCode n)
  modify (viLine . viCursorPos %~ (\x -> x - n))

eraseAndWrite
  :: forall fd effs . (ViwrapEff' fd effs) => ToHandle fd -> Int -> ByteString -> Eff effs ()
eraseAndWrite h n content = hWrite @fd h $ mconcat [BS.replicate n 8, content]
