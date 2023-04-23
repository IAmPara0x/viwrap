module Main
  ( main
  ) where

import Control.Concurrent         (MVar, newMVar, readMVar)
import Control.Monad              (void)
import Control.Monad.Freer        (runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State  (evalState)
import Data.Aeson qualified as Aeson
import Data.Default               (def)

import Lens.Micro                 ((&),(%~), (^.))

import System.Console.ANSI        qualified as ANSI
import System.Environment         (getArgs)
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals.Exts  qualified as Signals
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))

import Text.Printf                (printf)

import Viwrap (Initialized(..), pollMasterFd)
import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler (runTerminalIO)
import Capture.Viwrap.Pty.Handler (runHandleActTestIO,initialCaptureContents, CaptureContents(..), stdinContents,startTime, inputAt)
import Viwrap.Pty.TermSize        (getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.Signals             (passOnSignal, sigCHLDHandler, sigWINCHHandler)



initialise :: IO (Initialized, MVar CaptureContents)
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

                 , _logCtxs        = [OutputCtx,PollCtx]
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

  captureContentsMVar <- initialCaptureContents

  void $ Signals.installHandler Signals.sigCHLD
                                (sigCHLDHandler renv (cleanup renv captureContentsMVar))
                                (Just Signals.fullSignalSet)

  pure (Initialized { initialEnv = renv, slaveProcessHandle = ph, termStateMVar = mvar }, captureContentsMVar)


cleanup :: Env -> MVar CaptureContents -> IO ()
cleanup Env {..} captureContentsMVar = do

  let normalize :: CaptureContents -> CaptureContents
      normalize c = c & stdinContents %~ fmap (inputAt %~ (\i -> i - st)) 
        where
          st = c ^. startTime


  captureContents <- readMVar captureContentsMVar 
  Aeson.encodeFile "capture-content.json" (normalize captureContents)

  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)

  ANSI.hShowCursor IO.stdout

main :: IO ()
main = do

  (Initialized {..}, captureContentsMVar) <- initialise

  runHandleActTestIO pollMasterFd
    & evalState (def :: ViwrapState)
    & runTerminalIO
    & evalState termStateMVar
    & runLoggerIO
    & runReader initialEnv
    & evalState captureContentsMVar
    & runM
