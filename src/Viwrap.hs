module Viwrap
  ( Initialized (..)
  , cleanup
  , initialise
  , launch
  , pollMasterFd
  ) where

import Control.Concurrent         (MVar, newMVar)
import Control.Monad              (forever, void, when, zipWithM_)
import Control.Monad.Freer        (Eff, runM, sendM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.ByteString.Internal   qualified as BS
import Data.Default               (def)
import Data.Maybe                 (isNothing)

import Lens.Micro                 ((&), (.~))

import System.Console.ANSI        qualified as ANSI
import System.Environment         (getArgs)
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals.Exts  qualified as Signals
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             (ProcessHandle)

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runHandleActIO, runTerminalIO)
import Viwrap.Pty.TermSize        (getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.Signals             (passOnSignal, posixSignalHook, sigCHLDHandler, sigWINCHHandler)
import Viwrap.VI
import Viwrap.VI.Handler          (handleVIHook)
import Viwrap.VI.KeyMap           (defKeyMap, keyAction)


handleMaster :: ViwrapEff effs => Maybe ByteString -> Eff effs ()
handleMaster mcontent = do

  modify (isPromptUp .~ isNothing mcontent)

  case mcontent of
    Just content -> modify (prevMasterContent .~ content) >> writeStdout content
    Nothing      -> modify (prevMasterContent .~ mempty)
  handleVIHook

handleStdIn :: ViwrapEff effs => Maybe ByteString -> Eff effs ()
handleStdIn Nothing        = pure ()
handleStdIn (Just content) = do

  VIState { _viMode } <- _viState <$> get

  when (BS.singleton (BS.c2w '\EOT') == content) $ modify (recievedCtrlD .~ True)

  -- TODO: currently assume that the content is just a single character, however this may not be always true
  -- therefore we would require some sort of batch processing.

  when (content /= mempty) $ keyAction defKeyMap _viMode $ BS.head content

pollMasterFd :: forall effs . ViwrapEff effs => Eff effs ()
pollMasterFd = forever do

  stdin   <- getStdin
  hMaster <- snd . _masterPty <$> ask

  ViwrapState { _isPromptUp, _currentPollRate, _recievedCtrlD } <- get

  results <- if
    | _recievedCtrlD -> pure mempty
    | _isPromptUp    -> pselect [hMaster, stdin] Infinite
    | otherwise      -> pselect [hMaster, stdin] $ Wait _currentPollRate

  posixSignalHook
  zipWithM_ ($) [handleMaster, handleStdIn] results

data Initialized
  = Initialized
      { initialEnv         :: Env
      , slaveProcessHandle :: ProcessHandle
      , termStateMVar      :: MVar TermState
      }


initialise :: IO Initialized
initialise = do

  (cmd     , args   ) <- (\a -> (head a, tail a)) <$> getArgs

  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal
  hSetBuffering IO.stdin  NoBuffering
  hSetBuffering IO.stdout NoBuffering
  (hMaster, hSlave) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  size              <- ANSI.hGetTerminalSize IO.stdout
  cursorPos         <- ANSI.hGetCursorPosition IO.stdout
  mvar              <- newMVar TermState { _getTermSize       = size
                                         , _getTermCursorPos  = cursorPos
                                         , _getSignalReceived = Nothing
                                         }

  void $ Signals.installHandler Signals.sigWINCH (sigWINCHHandler mvar fdMaster) Nothing

  let renv :: Env
      renv = Env { _envCmd         = cmd
                 , _envCmdArgs     = args
                 , _envPollingRate = 20000
                 , _envBufferSize  = 2048
                 , _logFile        = "log.txt"
                 , _masterPty      = (fdMaster, hMaster)
                 , _slavePty       = (fdSlave, hSlave)

                 -- TODO: There should be two kinds of Env first one containing all the user config, Cmd, args
                 -- etc. The second Env would be created after we "setup" everything i.e. forked the process
                 -- install all the handlers, etc.
                 , _logCtxs        = [OutputCtx]
                 }

      setup = do

        sendM (setTermSize IO.stdInput fdMaster)
        newSize <- sendM $ getTermSize fdMaster
        logPty ["SetTermSize"] $ printf "FD: %s, New TermSize: %s" (show fdMaster) (show newSize)

        (ph, pid)      <- forkAndExecCmdIO

        masterTermAttr <- getTerminalAttrIO fdMaster
        setTerminalAttrIO IO.stdInput masterTermAttr Terminal.Immediately
        uninstallTerminalModes IO.stdInput [EnableEcho, ProcessInput] Terminal.Immediately

        pure (ph, pid)

  (ph, pid) <- runM $ runReader renv $ runLoggerIO setup

  void $ Signals.installHandler Signals.sigINT (passOnSignal pid Signals.sigINT mvar) Nothing

  void $ Signals.installHandler Signals.sigCHLD
                                (sigCHLDHandler renv (cleanup renv))
                                (Just Signals.fullSignalSet)

  pure $ Initialized { initialEnv = renv, slaveProcessHandle = ph, termStateMVar = mvar }

cleanup :: Env -> IO ()
cleanup Env {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)
  ANSI.hShowCursor IO.stdout

launch :: IO ()
launch = do
  Initialized {..} <- initialise

  runHandleActIO pollMasterFd
    & evalState (def :: ViwrapState)
    & runTerminalIO
    & evalState termStateMVar
    & runLoggerIO
    & runReader initialEnv
    & runM
