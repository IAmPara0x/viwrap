module Viwrap.Pty.Handler
  ( runHandleActIO
  , runLoggerIO
  , runLoggerUnit
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
import System.Posix               (Fd)
import System.Posix.IO.ByteString qualified as IO
import System.Timeout             qualified as IO
import Text.Printf                (printf)

import System.Process             qualified as Process
import Viwrap.Pty


-- Handlers for logger
runLoggerUnit :: forall a effs . Eff (Logger ': effs) a -> Eff effs a
runLoggerUnit = interpret $ \case
  LogM _ _ -> return ()

runLoggerIO
  :: forall a effs
   . (Members '[Reader (Env Fd)] effs, LastMember IO effs)
  => Eff (Logger ': effs) a
  -> Eff effs a
runLoggerIO = interpret $ \case
  LogM x (y : ys) -> do
    Env { _logFile } <- ask @(Env Fd)
    sendM $ appendFile _logFile $ (<> "\n") $ foldl (printf "%s, %s") (printf "[%s] %s" x y) ys
  LogM x [] -> do
    Env { _logFile } <- ask @(Env Fd)
    sendM $ appendFile _logFile $ printf "[%s]\n" x

-- Hanlder for 'HandleAct'
runHandleActIO
  :: forall a effs
   . (Members '[Logger , Reader (Env Fd)] effs, LastMember IO effs)
  => Eff (HandleAct Fd ': effs) a
  -> Eff effs a
runHandleActIO = interpret $ \case
  Pselect handles timeout -> pselectIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> return (IO.stdInput, stdin)
  GetStdout               -> return (IO.stdOutput, stdout)
  GetStderr               -> return (IO.stdError, stderr)

pselectIO
  :: forall effs
   . (Members '[Logger , Reader (Env Fd)] effs, LastMember IO effs)
  => [Handle]
  -> Timeout
  -> Eff effs [Maybe ByteString]
pselectIO handles timeout = do

  Env { _envBufferSize, _envPollingRate } <- ask @(Env Fd)

  let asyncReader :: Handle -> IO (Async (Maybe ByteString))
      asyncReader handle = async case timeout of
        Immediately ->
          (\c -> if c == BS.empty then Nothing else Just c)
            <$> BS.hGetNonBlocking handle _envBufferSize
        Infinite -> Just <$> reader handle
        Wait     -> IO.timeout _envPollingRate $ reader handle

      reader :: Handle -> IO ByteString
      reader handle = BS.hGetSome handle _envBufferSize

      tryReadContent :: Maybe (Either SomeException (Maybe ByteString)) -> IO (Maybe ByteString)
      tryReadContent = maybe (return Nothing) (either throwIO return)

      logSelect      = logM "PSelect"

  logSelect [printf "FDs: %s, Timeout: %s" (show handles) (show timeout)]
  results <- sendM do
    readers <- mapM asyncReader handles
    void $ waitAny readers
    results <- mapM (tryReadContent <=< poll) readers
    mapM_ cancel readers
    return results


  let fdAndContents :: [(Handle, ByteString)]
      fdAndContents = foldMap (\(h, r) -> maybe mempty (\c -> [(h, c)]) r) $ zip handles results

  logSelect [printf "Poll result: %s" (show fdAndContents)]
  return results

hWriteIO
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Handle
  -> ByteString
  -> Eff effs ()
hWriteIO handle content = do
  logM "FdWrite" [printf "writing %s to %s" (show content) (show handle)]
  sendM (BS.hPutStr handle content)

runProcessIO :: forall a effs . (LastMember IO effs) => Eff (Process ': effs) a -> Eff effs a
runProcessIO = interpret \case
  IsProcessDead ph -> sendM (Process.getProcessExitCode ph)
