module Test.Viwrap
  ( simulate
  ) where

import Control.Concurrent
import Control.Monad              (void)
import Control.Monad.Freer        (runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State  (evalState)

import Data.Default               (Default (def))

import Lens.Micro                 ((&))

import Simulate.Pty.Handler

import System.Console.ANSI        qualified as ANSI
import System.Environment         (getArgs)
import System.IO                  qualified as IO
import System.Posix               qualified as IO
import System.Posix.Signals       qualified as Signals
import System.Posix.Terminal      qualified as Terminal

import Text.Printf                (printf)

import Viwrap                     (Initialized (..), pollMasterFd)
import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runTerminalIO)
import Viwrap.Pty.TermSize        (getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.Signals             (sigCHLDHandler)



initialise :: IO Initialized
initialise = do

  (cmd     , args   ) <- (\a -> (head a, tail a)) <$> getArgs

  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal
  (hMaster , hSlave ) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  size                <- ANSI.hGetTerminalSize IO.stdout
  cursorPos           <- ANSI.hGetCursorPosition IO.stdout
  mvar                <- newMVar TermState { _getTermSize       = size
                                           , _getTermCursorPos  = cursorPos
                                           , _getSignalReceived = Nothing
                                           }

  let renv :: Env
      renv = Env { _envCmd         = cmd
                 , _envCmdArgs     = args
                 , _envPollingRate = 20000
                 , _envBufferSize  = 2048
                 , _logFile        = "log.txt"
                 , _masterPty      = (fdMaster, hMaster)
                 , _slavePty       = (fdSlave, hSlave)
                 , _logCtxs        = [PollCtx, OutputCtx]
                 }

      setup = do

        sendM (setTermSize IO.stdInput fdMaster)
        newSize <- sendM $ getTermSize fdMaster
        logPty ["SetTermSize"] $ printf "FD: %s, New TermSize: %s" (show fdMaster) (show newSize)

        (ph, pid)      <- forkAndExecCmdIO

        masterTermAttr <- getTerminalAttrIO fdMaster
        setTerminalAttrIO IO.stdInput masterTermAttr Terminal.Immediately
        uninstallTerminalModes IO.stdInput [IO.EnableEcho, IO.ProcessInput] Terminal.Immediately

        pure (ph, pid)


  (ph, _) <- runM $ runReader renv $ runLoggerIO setup

  pure $ Initialized { initialEnv = renv, slaveProcessHandle = ph, termStateMVar = mvar }


cleanup :: Env -> SimHandles -> IO ()
cleanup Env { _masterPty, _slavePty } SimHandles {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)
  mapM_ IO.hClose [fst _simStdIn, snd _simStdIn, _simStdOut, _simStdErr]
  ANSI.hShowCursor IO.stdout

simulate :: IO ()
simulate = do

  simHandles       <- mkSimHandles

  Initialized {..} <- initialise

  void $ Signals.installHandler Signals.sigCHLD
                                (sigCHLDHandler initialEnv (cleanup initialEnv simHandles))
                                (Just Signals.fullSignalSet)

  void $ forkIO $ feedStdIn "a\nexit()\n" simHandles

  runHandleActSimIO simHandles pollMasterFd
    & evalState (def :: ViwrapState)
    & runTerminalIO
    & evalState termStateMVar
    & runLoggerIO
    & runReader initialEnv
    & runM


