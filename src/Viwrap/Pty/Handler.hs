module Viwrap.Pty.Handler
  ( runHandleActIO
  , runProcessIO
  ) where

import Control.Concurrent.Async   (Async, async, cancel, poll, waitAny)
import Control.Exception          (SomeException, throwIO)
import Control.Monad              (void, (<=<))
import Control.Monad.Freer        (Eff, LastMember, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader, ask)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS

import System.IO                  (Handle, stderr, stdin, stdout)
import System.Posix.IO.ByteString qualified as IO
import System.Process             qualified as Process
import System.Timeout             qualified as IO
import System.Console.ANSI        qualified as ANSI

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty


-- Hanlder for 'HandleAct'
runHandleActIO
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActIO = interpret $ \case
  Pselect handles timeout -> pselectIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> return (IO.stdInput, stdin)
  GetStdout               -> return (IO.stdOutput, stdout)
  GetStderr               -> return (IO.stdError, stderr)
  HCursorPos handle       -> sendM $ ANSI.hGetCursorPosition handle
  HTerminalSize handle    -> sendM $ ANSI.hGetTerminalSize handle

pselectIO
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => [Handle]
  -> Timeout
  -> Eff effs [Maybe ByteString]
pselectIO handles timeout = do

  Env { _envBufferSize } <- ask

  let asyncReader :: Handle -> IO (Async (Maybe ByteString))
      asyncReader handle = async case timeout of
        Immediately ->
          (\c -> if c == BS.empty then Nothing else Just c)
            <$> BS.hGetNonBlocking handle _envBufferSize
        Infinite      -> Just <$> reader handle
        Wait pollRate -> IO.timeout pollRate $ reader handle

      reader :: Handle -> IO ByteString
      reader handle = BS.hGetSome handle _envBufferSize

      tryReadContent :: Maybe (Either SomeException (Maybe ByteString)) -> IO (Maybe ByteString)
      tryReadContent = maybe (return Nothing) (either throwIO return)

      logSelect      = logPoll ["PSelect"]

  logSelect $ printf "FDs: %s, Timeout: %s" (show handles) (show timeout)
  results <- sendM do
    readers <- mapM asyncReader handles
    void $ waitAny readers
    results <- mapM (tryReadContent <=< poll) readers
    mapM_ cancel readers
    return results


  let fdAndContents :: [(Handle, ByteString)]
      fdAndContents = foldMap (\(h, r) -> maybe mempty (\c -> [(h, c)]) r) $ zip handles results

  logSelect $ printf "Poll result: %s" (show fdAndContents)
  return results

hWriteIO
  :: (Members '[Logger] effs, LastMember IO effs)
  => Handle
  -> ByteString
  -> Eff effs ()
hWriteIO handle content = do
  logOutput ["FdWrite"] $ printf "writing %s to %s" (show content) (show handle)
  sendM (BS.hPutStr handle content)

runProcessIO :: (LastMember IO effs) => Eff (Process ': effs) a -> Eff effs a
runProcessIO = interpret \case
  IsProcessDead ph -> sendM (Process.getProcessExitCode ph)
