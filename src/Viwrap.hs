module Viwrap
  ( launch
  ) where

import Control.Concurrent         (MVar, newMVar, putMVar, takeMVar)
import Control.Monad              (void, when, zipWithM_)
import Control.Monad.Freer        (Eff, runM, sendM)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.ByteString.Internal   qualified as BS
import Data.Default               (def)
import Data.Maybe                 (isNothing)

import Lens.Micro                 ((&), (.~))

import System.IO                  qualified as IO
import System.IO                  (BufferMode (..), hSetBuffering)
import System.Posix               qualified as IO
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals       (Handler (Catch), Signal)
import System.Posix.Signals.Exts  qualified as Signals
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalMode (..))
import System.Process             (Pid, ProcessHandle)

import Text.Printf                (printf)

import System.Console.ANSI        qualified as ANSI
import System.Posix               (Fd)
import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runHandleActIO, runTerminalIO)
import Viwrap.Pty.TermSize        (TermSize (..), getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.VI
import Viwrap.VI.Handler          (handleVIHook)
import Viwrap.VI.KeyMap           (defKeyMap, keyAction)


handleDeadChild :: ViwrapEff effs => Eff effs ()
handleDeadChild = do

  hmaster <- snd . _masterPty <$> ask
  result  <- pselect [hmaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing      -> logOther ["childDied"] "Read all the output from slave process, exiting now."
    Just content -> writeStdout content >> handleDeadChild

handleMaster :: ViwrapEff effs => Maybe ByteString -> Eff effs ()
handleMaster mcontent = do

  modify (isPromptUp .~ isNothing mcontent)

  case mcontent of
    Just content -> modify (prevMasterContent .~ content) >> writeStdout content
    Nothing      -> modify (prevMasterContent .~ mempty)
  handleVIHook

handleStdIn :: ViwrapEff effs => Maybe ByteString -> Eff effs ()
handleStdIn Nothing        = pure ()
handleStdIn (Just content) = do

  VIState { _viMode } <- _viState <$> get

  when (BS.singleton (BS.c2w '\EOT') == content) $ modify (recievedCtrlD .~ True)

  -- TODO: currently assume that the content is just a single character, however this may not be always true
  -- therefore we would require some sort of batch processing.

  keyAction defKeyMap _viMode $ BS.head content

pollMasterFd :: forall effs . ViwrapEff effs => ProcessHandle -> Eff effs ()
pollMasterFd ph = do

  stdin   <- snd <$> getStdin
  hMaster <- snd . _masterPty <$> ask

  let poll :: Eff effs ()
      poll = do

        mExitCode <- isProcessDead ph

        case mExitCode of
          (Just exitCode) -> do
            logOther ["childDied"] $ printf "slave process exited with code: %s" (show exitCode)
            handleDeadChild
            modify (childStatus .~ Dead)

          Nothing -> do

            ViwrapState { _isPromptUp, _currentPollRate, _recievedCtrlD } <- get

            results <- if
              | _recievedCtrlD -> pselect [hMaster] Immediately
              | _isPromptUp    -> pselect [hMaster, stdin] Infinite
              | otherwise      -> pselect [hMaster, stdin] $ Wait _currentPollRate


            zipWithM_ ($) [handleMaster, handleStdIn] results

            poll
  poll


data Initialized
  = Initialized
      { initialEnv         :: Env
      , slaveProcessHandle :: ProcessHandle
      , termStateMVar      :: MVar TermState
      }

sigWINCHHandler :: MVar TermState -> Fd -> Handler
sigWINCHHandler termStateMVar fdMaster = Catch do
  setTermSize IO.stdInput fdMaster
  termState     <- takeMVar termStateMVar
  TermSize {..} <- getTermSize IO.stdInput
  putMVar termStateMVar (termState { _getTermSize = Just (termHeight, termWidth) })

passOnSignal :: Pid -> Signal -> Handler
passOnSignal pid signal = Catch do
  Signals.signalProcess signal pid


initialise :: IO Initialized
initialise = do
  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal
  hSetBuffering IO.stdin  NoBuffering
  hSetBuffering IO.stdout NoBuffering
  (hMaster, hSlave) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  size              <- ANSI.hGetTerminalSize IO.stdout
  cursorPos         <- ANSI.hGetCursorPosition IO.stdout
  mvar              <- newMVar TermState { _getTermSize = size, _getTermCursorPos = cursorPos }

  void $ Signals.installHandler Signals.sigWINCH (sigWINCHHandler mvar fdMaster) Nothing

  let renv :: Env
      renv = Env { _envCmd         = "ghci"
                 , _envCmdArgs     = []
                 , _envPollingRate = 20000
                 , _envBufferSize  = 2048
                 , _logFile        = "log.txt"
                 , _masterPty      = (fdMaster, hMaster)
                 , _slavePty       = (fdSlave, hSlave)

                 -- TODO: There should be two kinds of Env first one containing all the user config, Cmd, args
                 -- etc. The second Env would be created after we "setup" everything i.e. forked the process
                 -- install all the handlers, etc.
                 , _slavePid       = undefined
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

  (ph, pid) <- runM $ runReader renv $ runLoggerIO [PtyCtx] setup

  void $ Signals.installHandler Signals.sigINT (passOnSignal pid Signals.sigINT) Nothing

  pure $ Initialized { initialEnv         = renv { _slavePid = pid }
                     , slaveProcessHandle = ph
                     , termStateMVar      = mvar
                     }

cleanup :: Env -> IO ()
cleanup Env {..} = do
  IO.hClose (snd _masterPty)
  IO.hClose (snd _slavePty)
  ANSI.hShowCursor IO.stdout

launch :: IO ()
launch = do
  Initialized {..} <- initialise

  runHandleActIO (pollMasterFd slaveProcessHandle)
    & evalState (def :: ViwrapState)
    & runTerminalIO
    & evalState termStateMVar
    & runLoggerIO [OutputCtx, PollCtx, VICtx, OtherCtx]
    & runReader initialEnv
    & runM
  cleanup initialEnv
