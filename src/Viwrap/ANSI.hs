module Viwrap.ANSI
  ( ansiParser
  ) where

import Control.Monad            (void)
import Data.ByteString          (ByteString)
import Data.ByteString          qualified as BS
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.Void                (Void)
import Data.Word                (Word8)
import Text.Megaparsec

type Parser = Parsec Void ByteString

escapeStart :: Parser ByteString
escapeStart = chunk "\ESC"

csi :: Parser ByteString
csi = chunk (BS.singleton 91)

semicolon :: Parser ()
semicolon = void $ single $ c2w ';'

ctrlSeq :: Parser ByteString
ctrlSeq = BS.singleton <$> choice (map single [7 .. 13])

digitP :: Parser Word8
digitP = satisfy (isDigit . w2c)

cursorSeq :: Parser ByteString
cursorSeq = do
  escSeq <- sequence [escapeStart, csi]
  mline  <- optional $ try do
    line <- (foldMap BS.singleton) <$> some digitP
    semicolon
    return line

  columns <- (foldMap BS.singleton) <$> some digitP

  code    <- BS.singleton <$> choice (map (single . c2w) $ ['A' .. 'G'] ++ ['n'])

  case mline of
    Nothing     -> return $ mconcat $ escSeq ++ [columns, code]
    (Just line) -> return $ mconcat $ escSeq ++ [line, BS.singleton (c2w ';'), columns, code]


eraseSeq :: Parser ByteString
eraseSeq = do
  escSeq <- sequence [escapeStart, csi]
  mnum   <- optional digitP
  code   <- BS.singleton <$> choice (map (single . c2w) ['J', 'K'])

  case mnum of
    Nothing    -> return $ mconcat $ escSeq ++ [code]
    (Just num) -> return $ mconcat $ escSeq ++ [BS.singleton num, code]

ansiParser :: Parser ByteString
ansiParser = choice [try ctrlSeq, try eraseSeq, try cursorSeq]
