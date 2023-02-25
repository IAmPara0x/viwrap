module Viwrap
  ( launch
  ) where

import Control.Monad              (when, zipWithM_)
import Control.Monad.Freer        (Eff, runM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe                 (isNothing)

import Data.String                (fromString)
import Lens.Micro                 ((&), (.~))
import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix               (Fd)
import System.Posix.Terminal      qualified as Terminal

import System.Console.ANSI        qualified as ANSI
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             (ProcessHandle)
import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.Handler         (runHandleActIO, runLoggerIO, runProcessIO)
import Viwrap.Pty.Utils
import Viwrap.VI                  (VILine (..))
import Viwrap.VI.Handler          (handleVITerminal)
import Viwrap.VI.Utils            (defKeyMap, keyAction)


handleDeadChild :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleDeadChild = do

  hmaster <- snd . _masterPty <$> ask @(Env fd)
  result  <- pselect @fd [hmaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing        -> logM "childDied" ["Read all the output from slave process, exiting now."]
    (Just content) -> writeStdout @fd content >> handleDeadChild @fd

handleMaster :: forall fd effs . (ViwrapEff fd effs) => Maybe ByteString -> Eff effs ()
handleMaster mcontent = do

  modify (isPromptUp .~ isNothing mcontent)

  ViwrapState { _setCursorPos, _viLine = VILine {..} } <- get

  case mcontent of
    Just content -> writeStdout @fd content
    Nothing      -> when
      _setCursorPos
      do
        modify (setCursorPos .~ False)
        writeStdout @fd $ foldMap
          fromString
          [ ANSI.cursorBackwardCode $ BS.length _viLineContent - _viCursorPos
          , fromString ANSI.showCursorCode
          ]

pollMasterFd :: forall fd effs . (ViwrapEff fd effs) => ProcessHandle -> Eff effs ()
pollMasterFd ph = do

  let logPoll = logM "pollMasterFd"

  stdin   <- snd <$> getStdin @fd
  hMaster <- snd . _masterPty <$> ask @(Env fd)

  let handleStdIn :: Maybe ByteString -> Eff effs ()
      handleStdIn Nothing        = return ()
      handleStdIn (Just content) = do
        VILine {..} <- _viLine <$> get
        keyAction @fd (defKeyMap @fd) _viMode $ BS.head content

      poll :: Eff effs ()
      poll = do

        mExitCode <- isProcessDead ph

        case mExitCode of
          (Just exitCode) -> do
            logPoll [printf "slave process exited with code: %s" $ show exitCode]
            handleDeadChild @fd
            modify (childIsDead .~ True)

          Nothing -> do

            ViwrapState { _isPromptUp } <- get

            results <- pselect @fd [hMaster, stdin] $ if _isPromptUp then Infinite else Wait

            zipWithM_ ($) [handleMaster @fd, handleStdIn] results

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

launch :: IO ()
launch = do
  (renv, ph) <- initialise

  handleVITerminal @Fd (pollMasterFd @Fd ph)
    & runHandleActIO
    & runProcessIO
    & runLoggerIO
    & evalState initialViwrapState
    & runReader renv
    & runM
  cleanup renv
