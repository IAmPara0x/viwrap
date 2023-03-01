module Viwrap.VI.Utils
  ( addHook
  , autoCompleteP
  , eraseAndWrite
  , moveToBeginning
  , moveToEnd
    -- , moveToNextWord
  , removeHook
  , timeoutAndRemove
  , toMode
  ) where

import Control.Monad             (void)
import Control.Monad.Freer       (Eff, Members)
import Control.Monad.Freer.State (State, get, modify)

import Lens.Micro                ((%~), (.~), (^.))

import Data.ByteString           (ByteString)
import Data.ByteString           qualified as BS
-- import Data.ByteString.Internal  qualified as BS
-- import Data.Char                 (isAlphaNum)
import Data.Sequence             ((|>))
import Data.Sequence             qualified as Seq
import Data.Void                 (Void)
import System.IO                  (Handle)

import Text.Megaparsec           (Parsec, choice, eof, optional, some)
import Text.Megaparsec.Byte      (printChar, tab)
import Text.Printf               (printf)
import Viwrap.ANSI               (ansiParser)
import Viwrap.Logger
import Viwrap.Pty
import Viwrap.VI


moveToBeginning :: ViwrapEff effs => Eff effs ()
moveToBeginning = do
  VILine {..} <- _viLine <$> get
  void $ moveLeft (BS.length $ _viLineContent ^. zipperCrumbs)

moveToEnd :: ViwrapEff effs => Eff effs ()
moveToEnd = do
  VILine {..} <- _viLine <$> get
  void $ moveRight (BS.length $ _viLineContent ^. zipperFocus)

-- moveToNextWord :: ViwrapEff fd effs => Eff effs ()
-- moveToNextWord = do
--   VILine {..} <- _viLine <$> get

--   let content = BS.drop _viCursorPos _viLineContent
--       x       = BS.dropWhile (isAlphaNum . BS.w2c) content

--   void $ moveRight (BS.length content - BS.length x + 1)

toMode :: ViwrapEff effs => VIMode -> Eff effs ()
toMode mode = modify (viLine . viMode .~ mode)

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

eraseAndWrite
  :: (Members '[HandleAct] effs)
  => Handle
  -> Int
  -> ByteString
  -> Eff effs ()
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

