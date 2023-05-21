module Viwrap
  ( initialise
  , launch
  , pollMasterFd
  ) where

import Control.Concurrent         (newMVar)
import Control.Monad              (forever, void, when)
import Control.Monad.Freer        (Eff, runM, sendM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State  (evalState, get, modify)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.ByteString.Internal   qualified as BS
import Data.Default               (def)
import Data.Maybe                 (isNothing)

import Lens.Micro                 ((&), (.~), (^.))

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

import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runHandleActIO, runTerminalIO)
import Viwrap.Pty.TermSize        (getTermSize, setTermSize)
import Viwrap.Pty.Utils
import Viwrap.Signals             (passOnSignal, posixSignalHook, sigCHLDHandler, sigWINCHHandler)
import Viwrap.VI
import Viwrap.VI.Handler          (handleVIHook)
import Viwrap.VI.KeyMap           (defKeyMap, keyAction)


handleMaster :: Maybe ByteString -> Eff ViwrapStack ()
handleMaster mcontent = do

  modify (isPromptUp .~ isNothing mcontent)

  case mcontent of
    Just content -> modify (prevMasterContent .~ content) >> writeStdout content
    Nothing      -> modify (prevMasterContent .~ mempty)
  handleVIHook

handleStdIn :: Maybe ByteString -> Eff ViwrapStack ()
handleStdIn Nothing        = pure ()
handleStdIn (Just content) = do

  VIState { _viMode } <- _viState <$> get

  when (BS.singleton (BS.c2w '\EOT') == content) $ modify (recievedCtrlD .~ True)

  -- TODO: currently assume that the content is just a single character, however this may not be always true
  -- therefore we would require some sort of batch processing.
  keyAction defKeyMap _viMode $ BS.head content

pollMasterFd :: Eff ViwrapStack ()
pollMasterFd = forever do

  stdin   <- getStdin
  hMaster <- getMasterPty

  ViwrapState { _isPromptUp, _currentPollRate, _recievedCtrlD } <- get

  results <- if
    | _recievedCtrlD -> pure mempty
    | _isPromptUp    -> pselect [hMaster, stdin] Infinite
    | otherwise      -> pselect [hMaster, stdin] $ Wait _currentPollRate

  posixSignalHook

  let handleFileContent :: ViwrapHandle 'FileContent -> Eff ViwrapStack ()
      handleFileContent (StdIn     mcontent) = handleStdIn mcontent
      handleFileContent (MasterPty mcontent) = handleMaster mcontent
      handleFileContent h                    = logError ["pollMasterFd"]
        $ printf "unexpected viwrap handle %s, expected 'StdIn' or 'MasterPty'" (show h)

  mapM_ handleFileContent results

initialise :: IO Env
initialise = do

  (cmd     , args   ) <- (\a -> (head a, tail a)) <$> getArgs

  (fdMaster, fdSlave) <- Terminal.openPseudoTerminal

  (hMaster , hSlave ) <- (,) <$> IO.fdToHandle fdMaster <*> IO.fdToHandle fdSlave

  pure $ Env
    { _envCmd         = cmd
    , _envCmdArgs     = args
    , _envPollingRate = 20000
    , _envBufferSize  = 2048
    , _viwrapHandles  = ViwrapHandles { _masterPty = (fdMaster, MasterPty hMaster)
                                      , _slavePty  = (fdSlave, SlavePty hSlave)
                                      , _hStdIn    = StdIn IO.stdin
                                      , _hStdOut   = StdOut IO.stdout
                                      , _hStdErr   = StdErr IO.stderr
                                      }
    , _loggerConfig   = LoggerConfig { _logFile = "log.txt"
                                     , _logCtxs = [ErrorCtx, PollCtx, PtyCtx, OutputCtx]
                                     }
    }


launch :: IO ()
launch = do
  env <- initialise

  let stdin    = env ^. viwrapHandles . hStdIn
      stdout   = env ^. viwrapHandles . hStdOut
      stderr   = env ^. viwrapHandles . hStdErr
      fdMaster = fst $ env ^. viwrapHandles . masterPty


  mapM_ (onViwrapHandle $ flip hSetBuffering NoBuffering) [stdin, stdout, stderr]

  size          <- onViwrapHandle ANSI.hGetTerminalSize stdout
  cursorPos     <- onViwrapHandle ANSI.hGetCursorPosition stdout

  termStateMVar <- newMVar TermState { _getTermSize       = size
                                     , _getTermCursorPos  = cursorPos
                                     , _getSignalReceived = Nothing
                                     }
  setTermSize IO.stdInput fdMaster

  (_ph, pid) <- runM $ runReader env $ runLoggerIO do

    newSize <- sendM $ getTermSize fdMaster
    logPty ["SetTermSize"] $ printf "FD: %s, New TermSize: %s" (show fdMaster) (show newSize)

    (ph, pid)      <- forkAndExecCmdIO

    masterTermAttr <- getTerminalAttrIO fdMaster
    setTerminalAttrIO IO.stdInput masterTermAttr Terminal.Immediately
    uninstallTerminalModes IO.stdInput [EnableEcho, ProcessInput] Terminal.Immediately

    pure (ph, pid)

  let cleanup :: IO ()
      cleanup = do
        mapM_ (onViwrapHandle IO.hClose . snd)
              [env ^. viwrapHandles . masterPty, env ^. viwrapHandles . slavePty]
        ANSI.hShowCursor IO.stdout

  void $ Signals.installHandler Signals.sigWINCH (sigWINCHHandler termStateMVar fdMaster) Nothing
  void $ Signals.installHandler Signals.sigINT
                                (passOnSignal pid Signals.sigINT termStateMVar)
                                Nothing
  void $ Signals.installHandler Signals.sigCHLD
                                (sigCHLDHandler env cleanup)
                                (Just Signals.fullSignalSet)

  runHandleActIO pollMasterFd
    & runTerminalIO
    & evalState termStateMVar
    & evalState (def :: ViwrapState)
    & runLoggerIO
    & runReader env
    & runM
