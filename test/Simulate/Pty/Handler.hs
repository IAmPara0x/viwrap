module Simulate.Pty.Handler
  ( SimHandles (..)
  , feedStdIn
  , mkSimHandles
  , runHandleActSimIO
  ) where

import Control.Concurrent
import Control.Monad              (when)
import Control.Monad.Freer        (Eff, LastMember, Members, interpret)
import Control.Monad.Freer.Reader (Reader)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS

import System.IO                  qualified as IO
import System.IO                  (Handle)
import System.Process             qualified as Process

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Handler         (hWriteIO, pselectIO)


data SimHandles
  = SimHandles
      { _simStdIn  :: (Handle, Handle)
      , _simStdOut :: Handle
      , _simStdErr :: Handle
      }

mkSimHandles :: IO SimHandles
mkSimHandles = do
  (readH, writeH ) <- Process.createPipe
  (out  , hStdOut) <- IO.openTempFile "/tmp/" "stdout.viwrap"
  hStdErr          <- snd <$> IO.openTempFile "/tmp/" "stderr.viwrap"

  print out

  IO.hSetBuffering readH IO.NoBuffering
  IO.hSetBuffering writeH IO.NoBuffering
  IO.hSetBuffering hStdOut IO.NoBuffering
  IO.hSetBuffering hStdErr IO.NoBuffering

  pure $ SimHandles { _simStdIn = (readH, writeH), _simStdOut = hStdOut, _simStdErr = hStdErr }

runHandleActSimIO
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => SimHandles
  -> Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActSimIO simH = interpret $ \case
  Pselect handles timeout -> pselectIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> pure $ fst $ _simStdIn simH
  GetStdout               -> pure $ _simStdOut simH
  GetStderr               -> pure $ _simStdErr simH


feedStdIn :: ByteString -> SimHandles -> IO ()
feedStdIn inputBuffer simH = when
  (inputBuffer /= mempty)
  do

    let simStdIn = snd $ _simStdIn simH

    BS.hPutStr simStdIn (BS.take 1 inputBuffer)

    threadDelay 100000

    feedStdIn (BS.drop 1 inputBuffer) simH
