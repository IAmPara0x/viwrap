module Viwrap (launch) where

import Control.Monad (void)
import Control.Monad.Freer ( runM , sendM, Eff )
import Control.Monad.Freer.Reader (runReader)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSUTF8
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import Viwrap.Pty
import Viwrap.Pty.Handler (runLoggerIO, runLoggerUnit, runPtyActIO, ViwrapEffIO)
import System.Posix (Fd)
import System.Posix.Terminal (TerminalMode(..), TerminalState(..))
import System.Posix.Terminal qualified as Terminal
import System.Process (ProcessHandle)
import System.Process qualified as Process
import Text.Printf (printf)
import Control.Concurrent (threadDelay)


renv :: Env
renv = Env
    { envCmd = "python3"
    , envCmdArgs = []
    , envPollingRate = 10000
    , envBufferSize = 2048
    , logFile = "log.txt"
    }

pollMasterFd :: ViwrapEffIO effs => ProcessHandle -> HMaster -> Eff effs ()
pollMasterFd ph masterFd =
  do

    let logPollM = logM "PollMaster"

    mExitCode <- sendM (Process.getProcessExitCode ph)

    case mExitCode of
         (Just exitCode) -> logPollM [printf "slave process exited with code: %s" $ show exitCode]
         Nothing -> do
                      mcontent <- hRead masterFd
                      minput <- hRead stdin

                      maybe (logPollM ["masterFd read timeout."]) putStrBS mcontent

                      maybe (return ()) (hWrite masterFd) minput
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

       hMaster <- fdToHandle fdMaster
       hSlave <- fdToHandle fdSlave

       uninstallTerminalModes fdSlave [EnableEcho, ProcessInput] Immediately

       ph <- forkAndExecCmd hSlave

       mslavePID <- sendM $ Process.getPid ph
       logApp [printf "PID of slave process: %s" $ show mslavePID]


       () <- pollMasterFd ph hMaster

       void $ sendM (Process.waitForProcess ph)

       fdClose fdMaster

installTerminalModes :: forall effs. (ViwrapEffIO effs) => Fd -> [TerminalMode] -> TerminalState -> Eff effs ()
installTerminalModes fd modes state = do
                     termAttr <- getTerminalAttr fd
                     let newTermAttr = foldr (flip Terminal.withMode) termAttr modes

                     setTerminalAttr fd newTermAttr state

uninstallTerminalModes :: forall effs. (ViwrapEffIO effs) => Fd -> [TerminalMode] -> TerminalState -> Eff effs ()
uninstallTerminalModes fd modes state = do
                     termAttr <- getTerminalAttr fd
                     let newTermAttr = foldr (flip Terminal.withoutMode) termAttr modes

                     setTerminalAttr fd newTermAttr state


launch :: IO ()
launch = runM
       $ runReader renv
       $ runLoggerUnit
       $ runPtyActIO app
