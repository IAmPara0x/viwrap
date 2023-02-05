module Viwrap.Pty.Handler
  ( ViwrapEffIO
  , ViwrapEnv
  , runLoggerIO
  , runLoggerUnit
  , runPtyActIO
  ) where


import Control.Monad.Freer        (Eff, LastMember, Member, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader, ask)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import System.IO                  (Handle)
import System.Posix               (Fd)
import System.Posix.IO.ByteString qualified as IO
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalAttributes, TerminalMode (..), TerminalState)
import System.Process             qualified as Process
import System.Process             (CreateProcess (..), ProcessHandle, StdStream (..))
import System.Timeout             (timeout)
import Text.Printf                (printf)

import Viwrap.Pty

type ViwrapEffIO effs = (Members '[Logger , PtyAct , Reader Env] effs, LastMember IO effs)
type ViwrapEnv effs = Member (Reader Env) effs

runLoggerUnit :: forall a effs . Eff (Logger ': effs) a -> Eff effs a
runLoggerUnit = interpret $ \case
  LogM _ _ -> return ()

runLoggerIO
  :: forall a effs . (ViwrapEnv effs, LastMember IO effs) => Eff (Logger ': effs) a -> Eff effs a
runLoggerIO = interpret $ \case
  LogM x (y : ys) -> do
    Env { logFile } <- ask
    sendM $ appendFile logFile $ (<> "\n") $ foldl (printf "%s, %s") (printf "[%s] %s" x y) ys
  LogM x [] -> do
    Env { logFile } <- ask
    sendM $ appendFile logFile $ printf "[%s]\n" x


runPtyActIO
  :: forall a effs
   . (LastMember IO effs, ViwrapEnv effs, Member Logger effs)
  => Eff (PtyAct ': effs) a
  -> Eff effs a
runPtyActIO = interpret $ \case
  OpenPty -> openPtyIO
  FdToHandle fd ->
    logM "FdToHandle" [printf "convert fd %d to handle" (toInteger fd)] >> sendM (IO.fdToHandle fd)
  FdClose fd                                -> fdCloseIO fd
  HRead   handle                            -> hReadIO handle
  HWrite handle content                     -> hWriteIO handle content
  ForkAndExecCmd  handle                    -> forkAndExecCmdIO handle
  GetTerminalAttr handle                    -> getTerminalAttrIO handle
  SetTerminalAttr handle termAttr termState -> setTerminalAttrIO handle termAttr termState


openPtyIO
  :: forall effs . (LastMember IO effs, Member Logger effs) => Eff effs (FdMaster, FdSlave)
openPtyIO = do
  let logOpenPty = logM "OpenPty"
  logOpenPty ["opening pty..."]
  (masterFd, slaveFd) <- sendM Terminal.openPseudoTerminal

  logOpenPty [printf "Master: FD=%d, Slave: FD=%d" (toInteger masterFd) (toInteger slaveFd)]
  return (masterFd, slaveFd)

fdCloseIO :: forall effs . (LastMember IO effs, Member Logger effs) => Fd -> Eff effs ()
fdCloseIO fd = logM "FdClose" [printf "closing fd: %s" $ show fd] >> sendM (IO.closeFd fd)


hReadIO
  :: forall effs
   . (ViwrapEnv effs, LastMember IO effs, Member Logger effs)
  => Handle
  -> Eff effs (Maybe ByteString)
hReadIO handle = do
  logM "FdRead" [printf "reading fd: %s" $ show handle]
  Env { envBufferSize, envPollingRate } <- ask
  sendM (timeout envPollingRate $ BS.hGetSome handle envBufferSize)


hWriteIO
  :: forall effs . (LastMember IO effs, Member Logger effs) => Handle -> ByteString -> Eff effs ()
hWriteIO handle content = do
  logM "FdWrite" [printf "writing %s to %s" (show content) (show handle)]
  sendM (BS.hPutStr handle content)

forkAndExecCmdIO
  :: forall effs
   . (LastMember IO effs, ViwrapEnv effs, Member Logger effs)
  => HSlave
  -> Eff effs ProcessHandle
forkAndExecCmdIO sHandle = do
  Env { envCmd } <- ask
  logM "ForkAndExecCmd" [printf "starting process by executing %s cmd" envCmd]
  (_, _, _, ph) <- sendM $ Process.createProcess $ (Process.shell envCmd)
    { delegate_ctlc = True
    , std_err       = UseHandle sHandle
    , std_out       = UseHandle sHandle
    , std_in        = UseHandle sHandle
    }
  return ph

getTerminalAttrIO
  :: forall effs . (LastMember IO effs, Member Logger effs) => Fd -> Eff effs TerminalAttributes
getTerminalAttrIO fd = do

  termAttr <- sendM (Terminal.getTerminalAttributes fd)
  logM "GetTerminalAttr"
       [printf "Terminal attributes of Fd: %d is: %s" (toInteger fd) (showTerminalModes termAttr)]
  return termAttr


setTerminalAttrIO
  :: forall effs
   . (LastMember IO effs, Member Logger effs)
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
