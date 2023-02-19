module Viwrap.Pty.Utils
  ( fdCloseIO
  , forkAndExecCmdIO
  , getTermSizeIO
  , getTerminalAttrIO
  , installTerminalModes
  , setTermSizeIO
  , setTerminalAttrIO
  , uninstallTerminalModes
  ) where

import Control.Monad.Freer        (Eff, LastMember, Members, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import System.Posix               (Fd)
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalAttributes, TerminalMode (..), TerminalState)

import System.Process             qualified as Process
import System.Process             (CreateProcess (..), ProcessHandle, StdStream (..))

import System.Posix.IO.ByteString qualified as IO

import Text.Printf                (printf)
import Viwrap.Pty
import Viwrap.Pty.TermSize        (TermSize, getTermSize, setTermSize)

installTerminalModes
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
installTerminalModes fd modes state = do
  termAttr <- sendM $ Terminal.getTerminalAttributes fd
  let newTermAttr = foldl Terminal.withMode termAttr modes

  logM "installTerminalModes"
       [printf "Fd: %s, TerminalAttributes: %s" (show fd) (showTerminalModes newTermAttr)]

  sendM $ Terminal.setTerminalAttributes fd newTermAttr state

uninstallTerminalModes
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
uninstallTerminalModes fd modes state = do
  termAttr <- sendM $ Terminal.getTerminalAttributes fd
  let newTermAttr = foldl Terminal.withoutMode termAttr modes

  logM "uninstallTerminalModes"
       [printf "Fd: %s, TerminalAttributes: %s" (show fd) (showTerminalModes newTermAttr)]

  sendM $ Terminal.setTerminalAttributes fd newTermAttr state


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


fdCloseIO :: forall effs . (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs ()
fdCloseIO fd = logM "FdClose" [printf "closing fd: %s" $ show fd] >> sendM (IO.closeFd fd)

forkAndExecCmdIO
  :: forall effs
   . (Members '[Logger , Reader (Env Fd)] effs, LastMember IO effs)
  => Eff effs ProcessHandle
forkAndExecCmdIO = do
  Env { _envCmd, _envCmdArgs, _slavePty = (_, sHandle) } <- ask @(Env Fd)
  logM "ForkAndExecCmd" [printf "starting process by executing %s cmd" _envCmd]
  (_, _, _, ph) <-
    sendM $ Process.createProcess_ "slave process" $ (Process.proc _envCmd _envCmdArgs)
      { delegate_ctlc = True
      , std_err       = UseHandle sHandle
      , std_out       = UseHandle sHandle
      , std_in        = UseHandle sHandle
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
       [printf "FD: %s, Terminal Attributes: %s" (show fd) (showTerminalModes termAttr)]
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

getTermSizeIO
  :: forall effs . (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs TermSize
getTermSizeIO fd = do
  size <- sendM (getTermSize fd)
  logM "GetTermSize" [printf "FD: %s, TermSize: %s" (show fd) (show size)]
  return size

setTermSizeIO
  :: forall effs . (Members '[Logger] effs, LastMember IO effs) => Fd -> Fd -> Eff effs ()
setTermSizeIO fdFrom fdTo = do
  sendM (setTermSize fdFrom fdTo)
  newSize <- sendM $ getTermSize fdTo
  logM "SetTermSize" [printf "FD: %s, New TermSize: %s" (show fdTo) (show newSize)]

