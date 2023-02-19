module Viwrap
  ( launch
  ) where

-- import Data.String (fromString)
-- import System.Console.ANSI     qualified as ANSI
import Control.Monad              (when, zipWithM_)
import Control.Monad.Freer        (Eff, runM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)

import Lens.Micro                 ((&), (.~))
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix               (Fd)
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             (ProcessHandle)
import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.Handler         (runHandleActIO, runLoggerIO, runProcessIO)
import Viwrap.Pty.Utils


childDead :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
childDead = do

  stdout                            <- snd <$> getStdout @fd
  Env { _masterPty = (_, hMaster) } <- ask @(Env fd)
  result                            <- pselect @fd [hMaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing        -> logM "childDied" ["Read all the output from slave process, exiting now."]
    (Just content) -> hWrite @fd stdout content >> childDead @fd


pollMasterFd :: forall fd effs . (ViwrapEff fd effs) => ProcessHandle -> Eff effs ()
pollMasterFd ph = do

  let logPoll = logM "pollMasterFd"

  stdin                             <- snd <$> getStdin @fd
  stdout                            <- snd <$> getStdout @fd
  Env { _masterPty = (_, hMaster) } <- ask @(Env fd)

  let handleMaster :: Maybe ByteString -> Eff effs ()
      handleMaster (Just content) = do

        modify (isPromptUp .~ False)
        modify (prevMasterContent .~ content)
        hWrite @fd stdout content

      handleMaster Nothing = do
        ViwrapState { _prevMasterContent } <- get
        when (_prevMasterContent /= "\r\n") $ modify (isPromptUp .~ True)

      handleStdIn :: Maybe ByteString -> Eff effs ()
      handleStdIn Nothing        = return ()
      handleStdIn (Just content) = hWrite @fd hMaster content

      poll :: Eff effs ()
      poll = do

        mExitCode <- isProcessDead ph

        case mExitCode of
          (Just exitCode) -> do
            logPoll [printf "slave process exited with code: %s" $ show exitCode]
            childDead @fd
            modify (childIsDead .~ True)

          Nothing -> do

            ViwrapState { _isPromptUp } <- get

            results                     <- if _isPromptUp
              then pselect @fd [hMaster, stdin] Infinite
              else pselect @fd [hMaster, stdin] Wait

            zipWithM_ ($) [handleMaster, handleStdIn] results

            poll
  poll

initialise :: IO (Env Fd, ProcessHandle)
initialise = do
  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal
  hSetBuffering IO.stdin  NoBuffering
  hSetBuffering IO.stdout NoBuffering
  (hMaster, hSlave) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  let renv :: Env Fd
      renv = Env { _envCmd         = "python"
                 , _envCmdArgs     = []
                 , _envPollingRate = 10000
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

  ph <- runM $ runReader renv $ runLoggerIO setup

  return (renv, ph)

cleanup :: Env Fd -> IO ()
cleanup Env {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)

initialViwrapState :: ViwrapState
initialViwrapState =
  ViwrapState { _childIsDead = False, _isPromptUp = False, _prevMasterContent = mempty }

launch :: IO ()
launch = do
  (renv, ph) <- initialise

  runHandleActIO (pollMasterFd @Fd ph)
    & runProcessIO
    & runLoggerIO
    & evalState initialViwrapState
    & runReader renv
    & runM
  cleanup renv
