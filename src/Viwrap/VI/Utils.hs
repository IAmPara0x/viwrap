module Viwrap.VI.Utils
  ( Cursor (..)
  , addHook
  , autoCompleteP
  , eraseAndWrite
  , moveCursor
  , removeHook
  , timeoutAndRemove
  ) where

import Control.Monad.Freer        (Eff, Members)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State, get, modify)

import Lens.Micro                 ((%~))

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Sequence              ((|>))
import Data.Sequence              qualified as Seq
import Data.String                (fromString)
import Data.Void                  (Void)
import System.Console.ANSI        qualified as ANSI
import Text.Megaparsec            (Parsec, choice, eof, optional, some)
import Text.Megaparsec.Byte       (printChar, tab)
import Text.Printf                (printf)
import Viwrap.ANSI                (ansiParser)
import Viwrap.Logger              (Logger, logVI)
import Viwrap.Pty
import Viwrap.Pty.Utils           (writeStdout)
import Viwrap.VI


type Parser = Parsec Void ByteString

autoCompleteP :: Parser (Maybe AutoComplete)
autoCompleteP = do
  inputEnded <- optional eof

  let dropNonPrintableChar = do
        ansiseq <- ansiParser
        if ansiseq == "\n" then pure (Just CompletionList) else autoCompleteP

  case inputEnded of
    Nothing -> do
      mresult <- optional $ choice [some printChar, (: []) <$> tab]
      maybe dropNonPrintableChar (pure . Just . Completion . foldMap BS.singleton) mresult
    _ -> pure Nothing

eraseAndWrite
  :: (Members '[HandleAct] effs) => ViwrapHandle 'FileHandle -> Int -> ByteString -> Eff effs ()
eraseAndWrite h n content = hWrite h $ mconcat [BS.replicate n 127, content]

addHook :: (Members '[State ViwrapState , Logger] effs) => VIHook -> Eff effs ()
addHook hook = do
  hooks <- _viHooks <$> get
  logVI ["addHook"] $ printf "Adding %s to %s" (show hook) (show hooks)
  modify (viHooks %~ (|> hook))

removeHook :: (Members '[State ViwrapState] effs) => Eff effs ()
removeHook = modify (viHooks %~ Seq.drop 1)

timeoutAndRemove :: Eff ViwrapStack () -> Eff ViwrapStack ()
timeoutAndRemove runHook = do
  content <- _prevMasterContent <$> get
  if content == mempty then removeHook else runHook


data Cursor
  = Forward Int
  | Backward Int
  deriving stock (Eq, Show)


-- 'moveCursor' is the general function that moves the cursor to the desired position
-- in stdout, we need this function because to take into account the line wrapping which happens
-- when a single line is very long, hence in that case we have to much the cursor vertically and horizontally
-- as well
moveCursor :: Members '[HandleAct , Logger , Reader Env , Terminal] effs => Cursor -> Eff effs ()
moveCursor (Forward  0) = pure ()
moveCursor (Backward 0) = pure ()
moveCursor cursor       = do
  (height, width) <- termSize
  (row   , col  ) <- termCursorPos

  logVI ["moveCursor"]
    $ printf "cursor pos: %s, termsize: %s" (show (row, col)) (show (height, width))

  -- TODO: find a better way to solve this.
  let movePos = case cursor of
        Forward  n -> n
        Backward n -> n

      (vertMove, horizMove) = (movePos `div` width, movePos `mod` width)
      (nrow    , ncol     ) = case cursor of

        (Forward _)
          | col + horizMove >= width -> (row + vertMove + 1, (col + horizMove) `mod` width)
          | otherwise                -> (row + vertMove, col + horizMove)

        (Backward _) | col >= horizMove -> (row - vertMove, col - horizMove)
                     | otherwise        -> (row - vertMove - 1, col + width - horizMove)

  writeStdout (fromString $ ANSI.setCursorPositionCode nrow ncol)
