module Viwrap
  ( launch
  ) where

import Control.Monad              (void, when, zipWithM_)
import Control.Monad.Freer        (Eff, runM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS

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
import Viwrap.VI
import Viwrap.VI.Handler          (handleVITerminal)


childDead :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
childDead = do

  stdout                            <- snd <$> getStdout @fd
  Env { _masterPty = (_, hMaster) } <- ask @(Env fd)
  result                            <- pselect @fd [hMaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing        -> logM "childDied" ["Read all the output from slave process, exiting now."]
    (Just content) -> hWrite @fd stdout content >> childDead @fd


handleNewline :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleNewline = do

  hmaster <- snd . _masterPty <$> ask @(Env fd)
  stdout  <- snd <$> getStdout @fd

  hWrite @fd hmaster "\n"
  hWrite @fd stdout "\n"
  modify (viLine .~ initialVILine)

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
      handleStdIn (Just content) = do
        VILine {..} <- _viLine <$> get

        case _viMode of
          Normal | content == BS.singleton 104 -> void moveLeft
          Normal | content == BS.singleton 108 -> void moveRight
          Normal | content == BS.singleton 105 -> modify (viLine . viMode .~ Insert)
          Normal | content == BS.singleton 100 -> void backspace
          Normal | content == BS.singleton 10  -> handleNewline @fd
          Normal | otherwise                   -> return ()
          Insert | content == BS.singleton 10  -> handleNewline @fd
          Insert | content == BS.singleton 27  -> modify (viLine . viMode .~ Normal)
          Insert | content == BS.singleton 127 -> void backspace
          Insert | otherwise                   -> void $ insertBS content

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

            results <- pselect @fd [hMaster, stdin] $ if _isPromptUp then Infinite else Wait

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
        uninstallTerminalModes fdMaster    [EnableEcho]               Terminal.Immediately
        return ph

  ph <- runM $ runReader renv $ runLoggerIO setup

  return (renv, ph)

cleanup :: Env Fd -> IO ()
cleanup Env {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)


initialViwrapState :: ViwrapState
initialViwrapState = ViwrapState { _childIsDead       = False
                                 , _isPromptUp        = False
                                 , _prevMasterContent = mempty
                                 , _viLine            = initialVILine
                                 }

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
