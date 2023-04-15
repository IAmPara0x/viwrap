module Viwrap.Pty.Handler
  ( runHandleActIO
  , runTerminalIO
  ) where

import Control.Concurrent         (MVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Async   (Async, async, cancel, poll, waitAny)
import Control.Exception          (SomeException, throwIO)
import Control.Monad              (void, (<=<))
import Control.Monad.Freer        (Eff, LastMember, Members, interpret, reinterpret, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State  (State, get)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe                 (fromJust)

import System.Console.ANSI        qualified as ANSI
import System.IO                  (Handle, stderr, stdin, stdout)
import System.Posix.IO.ByteString qualified as IO
import System.Process             qualified as Process
import System.Timeout             qualified as IO

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Utils           (hGetCursorPosition)


-- Hanlder for 'HandleAct'
runHandleActIO
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActIO = interpret $ \case
  Pselect handles timeout -> pselectIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> pure (IO.stdInput, stdin)
  GetStdout               -> pure (IO.stdOutput, stdout)
  GetStderr               -> pure (IO.stdError, stderr)
  IsProcessDead ph        -> sendM (Process.getProcessExitCode ph)

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
      tryReadContent = maybe (pure Nothing) (either throwIO pure)

      logSelect      = logPoll ["PSelect"]

  logSelect $ printf "FDs: %s, Timeout: %s" (show handles) (show timeout)
  results <- sendM do
    readers <- mapM asyncReader handles
    void $ waitAny readers
    results <- mapM (tryReadContent <=< poll) readers
    mapM_ cancel readers
    pure results


  let fdAndContents :: [(Handle, ByteString)]
      fdAndContents = foldMap (\(h, r) -> maybe mempty (\c -> [(h, c)]) r) $ zip handles results

  logSelect $ printf "Poll result: %s" (show fdAndContents)
  pure results

hWriteIO :: (Members '[Logger] effs, LastMember IO effs) => Handle -> ByteString -> Eff effs ()
hWriteIO handle content = do
  logOutput ["FdWrite"] $ printf "writing %s to %s" (show content) (show handle)
  sendM (BS.hPutStr handle content)


runTerminalIO
  :: (Members '[Logger] effs, LastMember IO effs)
  => Eff (Terminal ': effs) a
  -> Eff (State (MVar TermState) ': effs) a
runTerminalIO = reinterpret $ \case
  TermSize -> do
    mTermSize <- fmap _getTermSize . sendM . readMVar =<< get
    case mTermSize of
      (Just size) -> pure size
      Nothing     -> fromJust <$> sendM (ANSI.hGetTerminalSize stdout)

  TermCursorPos -> do
    fromJust <$> hGetCursorPosition stdout
    -- mTermCursorPos <- fmap _getTermCursorPos . sendM . readMVar =<< get
    -- case mTermCursorPos of
    --   (Just pos) -> pure pos
    --   Nothing -> fromJust <$> sendM (ANSI.hGetCursorPosition stdout)

  CacheCursorPos pos -> do
    (termStateMVar :: MVar TermState) <- get
    termState                         <- sendM (takeMVar termStateMVar)
    sendM $ putMVar termStateMVar (termState { _getTermCursorPos = Just pos })

  ClearCacheCursorPos -> do
    (termStateMVar :: MVar TermState) <- get
    termState                         <- sendM (takeMVar termStateMVar)
    newPos                            <- sendM (ANSI.hGetCursorPosition stdout)
    sendM $ putMVar termStateMVar (termState { _getTermCursorPos = newPos })
