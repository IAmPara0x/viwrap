module Viwrap.Pty.Handler
  ( runHandleActIO
  , runLoggerIO
  , runLoggerUnit
  , runPtyActIO
  , runTerminalIO
  ) where

import Control.Monad.Freer        (Eff, LastMember, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader, ask)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import System.IO                  (Handle, stderr, stdin, stdout)
import System.Posix               (Fd)
import System.Posix.IO.ByteString qualified as IO
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalAttributes, TerminalMode (..), TerminalState)
import System.Process             qualified as Process
import System.Process             (CreateProcess (..), ProcessHandle, StdStream (..))
import System.Timeout             (timeout)
import Text.Printf                (printf)

import Viwrap.Pty

-- Handlers for logger
runLoggerUnit :: forall a effs . Eff (Logger ': effs) a -> Eff effs a
runLoggerUnit = interpret $ \case
  LogM _ _ -> return ()

runLoggerIO
  :: forall a effs
   . (Members '[Reader Env] effs, LastMember IO effs)
  => Eff (Logger ': effs) a
  -> Eff effs a
runLoggerIO = interpret $ \case
  LogM x (y : ys) -> do
    Env { logFile } <- ask
    sendM $ appendFile logFile $ (<> "\n") $ foldl (printf "%s, %s") (printf "[%s] %s" x y) ys
  LogM x [] -> do
    Env { logFile } <- ask
    sendM $ appendFile logFile $ printf "[%s]\n" x

-- handler for 'PtyAct'
runPtyActIO
  :: forall a effs
   . (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Eff (PtyAct ': effs) a
  -> Eff effs a
runPtyActIO = interpret $ \case
  OpenPty -> openPtyIO
  FdToHandle fd ->
    logM "FdToHandle" [printf "convert fd %d to handle" (toInteger fd)] >> sendM (IO.fdToHandle fd)
  FdClose         fd                        -> fdCloseIO fd
  ForkAndExecCmd  handle                    -> forkAndExecCmdIO handle
  GetTerminalAttr handle                    -> getTerminalAttrIO handle
  SetTerminalAttr handle termAttr termState -> setTerminalAttrIO handle termAttr termState

openPtyIO
  :: forall effs . (Members '[Logger] effs, LastMember IO effs) => Eff effs (FdMaster, FdSlave)
openPtyIO = do

  let logOpenPty = logM "OpenPty"
  logOpenPty ["opening pty..."]

  (masterFd, slaveFd) <- sendM Terminal.openPseudoTerminal

  logOpenPty [printf "Master: FD=%d, Slave: FD=%d" (toInteger masterFd) (toInteger slaveFd)]
  return (masterFd, slaveFd)

fdCloseIO :: forall effs . (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs ()
fdCloseIO fd = logM "FdClose" [printf "closing fd: %s" $ show fd] >> sendM (IO.closeFd fd)

forkAndExecCmdIO
  :: forall effs
   . (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => HSlave
  -> Eff effs ProcessHandle
forkAndExecCmdIO sHandle = do
  Env { envCmd } <- ask
  logM "ForkAndExecCmd" [printf "starting process by executing %s cmd" envCmd]
  (_, _, _, ph) <- sendM $ Process.createProcess_ "slave process" $ (Process.shell envCmd)
    { delegate_ctlc = True
    , std_err       = UseHandle sHandle
    , std_out       = UseHandle sHandle
    , std_in        = UseHandle sHandle
    -- , new_session   = True
    }
  return ph

getTerminalAttrIO
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> Eff effs TerminalAttributes
getTerminalAttrIO fd = do

  termAttr <- sendM (Terminal.getTerminalAttributes fd)
  logM "GetTerminalAttr"
       [printf "Terminal attributes of Fd: %d is: %s" (toInteger fd) (showTerminalModes termAttr)]
  return termAttr


setTerminalAttrIO
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> TerminalAttributes
  -> TerminalState
  -> Eff effs ()
setTerminalAttrIO fd termAttr termState = do
  logM
    "SetTerminalAttr"
    [ printf "Setting terminal attributes: %s, for fd: %d"
             (showTerminalModes termAttr)
             (toInteger fd)
    ]
  sendM (Terminal.setTerminalAttributes fd termAttr termState)

-- Hanlder for 'HandleAct'
runHandleActIO
  :: forall a effs
   . (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActIO = interpret $ \case
  HRead handle          -> hReadIO handle
  HWrite handle content -> hWriteIO handle content

hReadIO
  :: forall effs
   . (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Handle
  -> Eff effs (Maybe ByteString)
hReadIO handle = do

  let logRead = logM "HRead"

  logRead [printf "reading fd: %s" $ show handle]
  Env { envBufferSize, envPollingRate } <- ask
  sendM (timeout envPollingRate $ BS.hGetSome handle envBufferSize)

hWriteIO
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Handle
  -> ByteString
  -> Eff effs ()
hWriteIO handle content = do
  logM "FdWrite" [printf "writing %s to %s" (show content) (show handle)]
  sendM (BS.hPutStr handle content)


-- Handler for 'Terminal'
runTerminalIO :: forall a effs . Eff (Terminal ': effs) a -> Eff effs a
runTerminalIO = interpret $ \case
  GetStdin  -> return (IO.stdInput, stdin)
  GetStdout -> return (IO.stdOutput, stdout)
  GetStderr -> return (IO.stdError, stderr)

allTerminalModes :: [(String, TerminalMode)]
allTerminalModes =
  [ ("InterruptOnBreak"        , InterruptOnBreak)
  , ("MapCRtoLF"               , MapCRtoLF)
  , ("IgnoreBreak"             , IgnoreBreak)
  , ("IgnoreCR"                , IgnoreCR)
  , ("IgnoreParityErrors"      , IgnoreParityErrors)
  , ("MapLFtoCR"               , MapLFtoCR)
  , ("CheckParity"             , CheckParity)
  , ("StripHighBit"            , StripHighBit)
  , ("StartStopInput"          , StartStopInput)
  , ("StartStopOutput"         , StartStopOutput)
  , ("MarkParityErrors"        , MarkParityErrors)
  , ("ProcessOutput"           , ProcessOutput)
  , ("LocalMode"               , LocalMode)
  , ("ReadEnable"              , ReadEnable)
  , ("TwoStopBits"             , TwoStopBits)
  , ("HangupOnClose"           , HangupOnClose)
  , ("EnableParity"            , EnableParity)
  , ("OddParity"               , OddParity)
  , ("EnableEcho"              , EnableEcho)
  , ("EchoErase"               , EchoErase)
  , ("EchoKill"                , EchoKill)
  , ("EchoLF"                  , EchoLF)
  , ("ProcessInput"            , ProcessInput)
  , ("ExtendedFunctions"       , ExtendedFunctions)
  , ("KeyboardInterrupts"      , KeyboardInterrupts)
  , ("NoFlushOnInterrupt"      , NoFlushOnInterrupt)
  , ("BackgroundWriteInterrupt", BackgroundWriteInterrupt)
  ]

showTerminalModes :: TerminalAttributes -> String
showTerminalModes termAttr = printf "Terminal Modes: %s" (show $ foldr acc [] allTerminalModes)
 where
  acc :: (String, TerminalMode) -> [String] -> [String]
  acc (name, mode) names | Terminal.terminalMode mode termAttr = name : names
                         | otherwise                           = names
