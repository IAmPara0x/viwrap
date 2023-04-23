module Capture.Viwrap
  ( main
  ) where

import CmdArgs                    (CaptureArgs (..))
import Control.Concurrent         (MVar, newMVar, readMVar)
import Control.Monad              (void)
import Control.Monad.Freer        (runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State  (evalState)

import Data.Aeson                 qualified as Aeson
import Data.Default               (def)
import Data.Text.IO               qualified as Text

import Lens.Micro                 ((%~), (&), (^.))

import System.Console.ANSI        qualified as ANSI
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals.Exts  qualified as Signals
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))

import Text.Printf                (printf)

import Capture.Viwrap.Pty.Handler
  ( CaptureContents (..)
  , initialCaptureContents
  , inputAt
  , runHandleActTestIO
  , startTime
  , stdinContents
  , stdoutContents
  )
import Viwrap                     (Initialized (..), pollMasterFd)
import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runTerminalIO)
import Viwrap.Pty.TermSize        (getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.Signals             (passOnSignal, sigCHLDHandler, sigWINCHHandler)



initialise :: FilePath -> String -> [String] -> IO (Initialized, MVar CaptureContents)
initialise captureFilePath cmd args = do

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
                 , _logCtxs        = [OutputCtx, PollCtx]
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

  void $ Signals.installHandler
    Signals.sigCHLD
    (sigCHLDHandler renv (cleanup captureFilePath renv captureContentsMVar))
    (Just Signals.fullSignalSet)

  pure
    ( Initialized { initialEnv = renv, slaveProcessHandle = ph, termStateMVar = mvar }
    , captureContentsMVar
    )


cleanup :: FilePath -> Env -> MVar CaptureContents -> IO ()
cleanup captureFilePath Env {..} captureContentsMVar = do

  captureContents <- readMVar captureContentsMVar


  let normalize = captureContents & stdinContents %~ helper

      helper inputs =
        zipWith (\i t -> i & inputAt %~ (+ negate t)) inputs (st : map (^. inputAt) inputs)
      st = captureContents ^. startTime

  Aeson.encodeFile (captureFilePath <> ".json") normalize
  Text.writeFile (captureFilePath <> ".golden") (mconcat $ captureContents ^. stdoutContents)

  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)

  ANSI.hShowCursor IO.stdout

main :: CaptureArgs -> IO ()
main CaptureArgs {..} = do

  (Initialized {..}, captureContentsMVar) <- initialise outputCaptureContentsFilePath
                                                        captureProgram
                                                        captureProgramArgs

  runHandleActTestIO pollMasterFd
    & evalState (def :: ViwrapState)
    & runTerminalIO
    & evalState termStateMVar
    & runLoggerIO
    & runReader initialEnv
    & evalState captureContentsMVar
    & runM
