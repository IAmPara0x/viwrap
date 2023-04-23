module Simulate.Viwrap.Pty.Handler
  ( SimHandles (..)
  , feedStdIn
  , mkSimHandles
  , runHandleActSimIO
  ) where

import Control.Concurrent
import Control.Monad.Freer        (Eff, LastMember, Members, interpret)
import Control.Monad.Freer.Reader (Reader)

import Data.ByteString            qualified as BS

import Data.Text.Encoding         (encodeUtf8)

import System.IO                  qualified as IO
import System.IO                  (Handle)
import System.Process             qualified as Process


import Capture.Viwrap.Pty.Handler (UserInput (..))
import CmdArgs                    (SimulateArgs (..))
import Data.Time                  (nominalDiffTimeToSeconds)
import Text.Printf                (printf)
import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Handler         (hWriteIO, pselectIO)


data SimHandles
  = SimHandles
      { _simStdIn  :: (Handle, Handle)
      , _simStdOut :: Handle
      , _simStdErr :: Handle
      }

mkSimHandles :: SimulateArgs -> IO SimHandles
mkSimHandles SimulateArgs {..} = do
  (readH, writeH) <- Process.createPipe
  hStdOut         <- IO.openFile (captureContentsFilePath <> ".test") IO.ReadWriteMode
  hStdErr         <- snd <$> IO.openTempFile "/tmp/" "stderr.viwrap"

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


feedStdIn :: [UserInput] -> SimHandles -> IO ()
feedStdIn []                        _    = pure ()
feedStdIn (UserInput {..} : inputs) simH = do

  let simStdIn = snd $ _simStdIn simH
      content  = encodeUtf8 _inputContents
      delay    = ceiling @_ @Int (nominalDiffTimeToSeconds _inputAt * 1000000)

  putStrLn (printf "Feeding input %s with a delay of %s" (show content) (show delay))

  threadDelay delay

  BS.hPutStr simStdIn content

  feedStdIn inputs simH
