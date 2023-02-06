module Viwrap
  ( launch
  ) where

import Control.Monad.Freer        (Eff, LastMember, Members, runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Function              ((&))

import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               (Fd)
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..), TerminalState (..))
import System.Process             qualified as Process
import System.Process             (ProcessHandle)
import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.Handler         (runHandleActIO, runLoggerIO, runPtyActIO, runTerminalIO)


type ViwrapEff effs = Members '[HandleAct , Logger , PtyAct , Terminal] effs

renv :: Env
renv = Env { envCmd         = "python"
           , envCmdArgs     = []
           , envPollingRate = 10000
           , envBufferSize  = 2048
           , logFile        = "log.txt"
           }

pollMasterFd :: (ViwrapEff effs, LastMember IO effs) => ProcessHandle -> HMaster -> Eff effs ()
pollMasterFd ph hMaster = do

  let logPollM = logM "PollMaster"
  stdin <- snd <$> getStdin

  let poll = do
        mExitCode <- sendM (Process.getProcessExitCode ph)
        case mExitCode of
          (Just exitCode) -> logPollM [printf "slave process exited with code: %s" $ show exitCode]
          Nothing         -> do
            mcontent <- hRead hMaster
            minput   <- hRead stdin

            maybe (logPollM ["masterFd read timeout."])
                  (\x -> if BS.null x then logPollM ["recieved null."] else putStrBS x)
                  mcontent

            maybe (return ()) (hWrite hMaster) minput
            pollMasterFd ph hMaster
  poll


putStrBS :: ViwrapEff effs => ByteString -> Eff effs ()
putStrBS content = do
  let logPutStr = logM "putStrBS"

  stdout <- snd <$> getStdout
  logPutStr [printf "writting \"%s\" of %d bytes to stdout" (show content) (BS.length content)]

  hWrite stdout content


app :: (ViwrapEff effs, LastMember IO effs) => Eff effs ()
app = do

  (fdStdin, hStdin ) <- getStdin
  (_      , hStdout) <- getStdout

  sendM (hSetBuffering hStdin NoBuffering)
  sendM (hSetBuffering hStdout NoBuffering)

  (fdMaster, fdSlave) <- openPty

  hMaster             <- fdToHandle fdMaster
  hSlave              <- fdToHandle fdSlave

  uninstallTerminalModes fdStdin [EnableEcho, ProcessInput] Immediately

  ph <- forkAndExecCmd hSlave


  () <- pollMasterFd ph hMaster

  fdClose fdMaster

installTerminalModes
  :: forall effs
   . (Members '[Logger , PtyAct] effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
installTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldr (flip Terminal.withMode) termAttr modes

  setTerminalAttr fd newTermAttr state

uninstallTerminalModes
  :: forall effs
   . (Members '[Logger , PtyAct] effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
uninstallTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldr (flip Terminal.withoutMode) termAttr modes

  setTerminalAttr fd newTermAttr state

launch :: IO ()
launch = runPtyActIO app & runTerminalIO & runHandleActIO & runLoggerIO & runReader renv & runM
