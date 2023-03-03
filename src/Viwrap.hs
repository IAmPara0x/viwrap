module Viwrap
  ( launch
  ) where

import Control.Monad              (zipWithM_)
import Control.Monad.Freer        (Eff, runM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe                 (isNothing)

import Lens.Micro                 ((&), (.~))

import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             (ProcessHandle)

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty
import Viwrap.Pty.Handler         (runHandleActIO, runProcessIO)
import Viwrap.Pty.Utils
import Viwrap.VI                  (VILine (..))
import Viwrap.VI.Handler          (handleVIHook, handleVITerminal)
import Viwrap.VI.KeyMap           (defKeyMap, keyAction)


handleDeadChild :: ViwrapEff effs => Eff effs ()
handleDeadChild = do

  hmaster <- snd . _masterPty <$> ask
  result  <- pselect [hmaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing      -> logOther ["childDied"] "Read all the output from slave process, exiting now."
    Just content -> writeStdout content >> handleDeadChild

handleMaster :: ViwrapEff effs => Maybe ByteString -> Eff effs ()
handleMaster mcontent = do

  modify (isPromptUp .~ isNothing mcontent)

  case mcontent of
    Just content -> modify (prevMasterContent .~ content) >> writeStdout content
    Nothing      -> modify (prevMasterContent .~ mempty)
  handleVIHook

pollMasterFd :: forall effs . ViwrapEff effs => ProcessHandle -> Eff effs ()
pollMasterFd ph = do

  stdin   <- snd <$> getStdin
  hMaster <- snd . _masterPty <$> ask

  let handleStdIn :: Maybe ByteString -> Eff effs ()
      handleStdIn Nothing        = return ()
      handleStdIn (Just content) = do
        VILine {..} <- _viLine <$> get
        keyAction defKeyMap _viMode $ BS.head content

      poll :: Eff effs ()
      poll = do

        mExitCode <- isProcessDead ph

        case mExitCode of
          (Just exitCode) -> do
            logOther ["childDied"] $ printf "slave process exited with code: %s" (show exitCode)
            handleDeadChild
            modify (childStatus .~ Dead)

          Nothing -> do

            ViwrapState { _isPromptUp, _currentPollRate } <- get

            results <- pselect [hMaster, stdin]
              $ if _isPromptUp then Infinite else Wait _currentPollRate

            zipWithM_ ($) [handleMaster, handleStdIn] results

            poll
  poll

initialise :: IO (Env, ProcessHandle)
initialise = do
  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal
  hSetBuffering IO.stdin  NoBuffering
  hSetBuffering IO.stdout NoBuffering
  (hMaster, hSlave) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  let renv :: Env
      renv = Env { _envCmd         = "node"
                 , _envCmdArgs     = []
                 , _envPollingRate = 20000
                 , _envBufferSize  = 2048
                 , _logFile        = "log.txt"
                 , _masterPty      = (fdMaster, hMaster)
                 , _slavePty       = (fdSlave, hSlave)
                 }

      setup = do
        setTermSizeIO IO.stdInput fdMaster

        ph             <- forkAndExecCmdIO

        masterTermAttr <- getTerminalAttrIO fdMaster
        setTerminalAttrIO IO.stdInput masterTermAttr Terminal.Immediately
        uninstallTerminalModes IO.stdInput [EnableEcho, ProcessInput] Terminal.Immediately

        return ph

  ph <- runM $ runReader renv $ runLoggerIO [PtyCtx] setup

  return (renv, ph)

cleanup :: Env -> IO ()
cleanup Env {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)

launch :: IO ()
launch = do
  (renv, ph) <- initialise

  handleVITerminal (pollMasterFd ph)
    & runHandleActIO
    & runProcessIO
    & runLoggerIO [PollCtx, VICtx]
    & evalState initialViwrapState
    & runReader renv
    & runM
  cleanup renv
