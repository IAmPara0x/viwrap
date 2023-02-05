module Viwrap
  ( launch
  ) where

import Control.Monad              (void)
import Control.Monad.Freer        (Eff, runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.ByteString.UTF8       qualified as BSUTF8
import Data.Function              ((&))

import System.IO                  (BufferMode (..), hSetBuffering, stdin, stdout)
import System.Posix               qualified as IO
import System.Posix               (Fd)
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..), TerminalState (..))
import System.Process             qualified as Process
import System.Process             (ProcessHandle)
import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.Handler         (ViwrapEffIO, runLoggerIO, runLoggerUnit, runPtyActIO)


renv :: Env
renv = Env { envCmd         = "python3"
           , envCmdArgs     = []
           , envPollingRate = 10000
           , envBufferSize  = 2048
           , logFile        = "log.txt"
           }

pollMasterFd :: ViwrapEffIO effs => ProcessHandle -> HMaster -> Eff effs ()
pollMasterFd ph masterFd = do

  let logPollM = logM "PollMaster"

  mExitCode <- sendM (Process.getProcessExitCode ph)

  case mExitCode of
    (Just exitCode) -> logPollM [printf "slave process exited with code: %s" $ show exitCode]
    Nothing         -> do
      mcontent <- hRead masterFd
      minput   <- hRead stdin

      maybe (logPollM ["masterFd read timeout."]) putStrBS          mcontent

      maybe (return ())                           (hWrite masterFd) minput
      pollMasterFd ph masterFd


putStrBS :: ViwrapEffIO effs => ByteString -> Eff effs ()
putStrBS content = do
  let logPutStr = logM "putStrBS"
  logPutStr [printf "writting \"%s\" of %d bytes to stdout" (show content) (BS.length content)]
  sendM $ putStr (BSUTF8.toString content)


app :: ViwrapEffIO effs => Eff effs ()
app = do
  let logApp = logM "App"

  sendM (hSetBuffering stdout NoBuffering)
  sendM (hSetBuffering stdin NoBuffering)
  (fdMaster, fdSlave) <- openPty

  hMaster             <- fdToHandle fdMaster
  hSlave              <- fdToHandle fdSlave

  uninstallTerminalModes IO.stdInput [EnableEcho, ProcessInput] Immediately

  ph        <- forkAndExecCmd hSlave

  mslavePID <- sendM $ Process.getPid ph
  logApp [printf "PID of slave process: %s" $ show mslavePID]


  () <- pollMasterFd ph hMaster

  void $ sendM (Process.waitForProcess ph)

  fdClose fdMaster

installTerminalModes
  :: forall effs . (ViwrapEffIO effs) => Fd -> [TerminalMode] -> TerminalState -> Eff effs ()
installTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldr (flip Terminal.withMode) termAttr modes

  setTerminalAttr fd newTermAttr state

uninstallTerminalModes
  :: forall effs . (ViwrapEffIO effs) => Fd -> [TerminalMode] -> TerminalState -> Eff effs ()
uninstallTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldr (flip Terminal.withoutMode) termAttr modes

  setTerminalAttr fd newTermAttr state

launch :: IO ()
launch = runPtyActIO app & runLoggerUnit & runReader renv & runM
