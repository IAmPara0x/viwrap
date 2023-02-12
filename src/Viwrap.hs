module Viwrap
  ( launch
  ) where

import Control.Monad.Freer        (Eff, LastMember, runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Data.Function              ((&))

import System.Exit                (ExitCode)
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), Handle, hSetBuffering)
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             qualified as Process
import System.Process             (ProcessHandle)
import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.Handler
  ( runHandleActIO
  , runLoggerIO
  , runLoggerUnit
  , runPtyActIO
  , runTerminalIO
  )
import Viwrap.Pty.Utils           (uninstallTerminalModes)


renv :: Env
renv = Env { envCmd         = "python"
           , envCmdArgs     = []
           , envPollingRate = 10000
           , envBufferSize  = 2048
           , logFile        = "log.txt"
           }


childDied :: (ViwrapEff effs, LastMember IO effs) => ExitCode -> Handle -> Eff effs ()
childDied exitCode hMaster = do

  stdout <- snd <$> getStdout


  result <- pselect [hMaster] Wait

  let [mMasterContent] = result

  case mMasterContent of
    Nothing        -> return ()
    (Just content) -> hWrite stdout content >> childDied exitCode hMaster


pollMasterFd :: (ViwrapEff effs, LastMember IO effs) => ProcessHandle -> Handle -> Eff effs ()
pollMasterFd ph hMaster = do

  stdin  <- snd <$> getStdin
  stdout <- snd <$> getStdout

  let logPoll = logM "pollMasterFd"
      poll    = do
        mExitCode <- sendM (Process.getProcessExitCode ph)

        case mExitCode of
          (Just exitCode) ->
            logPoll [printf "slave process exited with code: %s" $ show exitCode]
              >> childDied exitCode hMaster
          Nothing -> do

            results <- pselect [hMaster, stdin] Wait

            let [mMasterContent, mStdinContent] = results

            maybe (return ()) (hWrite stdout)  mMasterContent
            maybe (return ()) (hWrite hMaster) mStdinContent

            pollMasterFd ph hMaster
  poll


app :: (ViwrapEff effs, LastMember IO effs) => Eff effs ()
app = do

  (fdStdin, hStdin ) <- getStdin
  (_      , hStdout) <- getStdout

  sendM (hSetBuffering hStdin NoBuffering)
  sendM (hSetBuffering hStdout NoBuffering)

  (fdMaster, fdSlave) <- openPty
  (hMaster , hSlave ) <- (,) <$> fdToHandle fdMaster <*> fdToHandle fdSlave

  setTermSize fdStdin fdMaster

  ph             <- forkAndExecCmd hSlave

  masterTermAttr <- getTerminalAttr fdMaster
  setTerminalAttr fdStdin masterTermAttr Terminal.Immediately
  uninstallTerminalModes fdStdin [EnableEcho, ProcessInput] Terminal.Immediately


  () <- pollMasterFd ph hMaster

  sendM (IO.hClose hSlave)
  sendM (IO.hClose hMaster)

launch :: IO ()
launch = runPtyActIO app & runTerminalIO & runHandleActIO & runLoggerIO & runReader renv & runM
