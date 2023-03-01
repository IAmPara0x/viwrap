module Viwrap.Pty.Utils
  ( fdCloseIO
  , forkAndExecCmdIO
  , getTermSizeIO
  , getTerminalAttrIO
  , installTerminalModes
  , setTermSizeIO
  , setTerminalAttrIO
  , uninstallTerminalModes
  , writeMaster
  , writeStdout
  ) where

import Control.Monad.Freer        (Eff, LastMember, Members, sendM)
import Control.Monad.Freer.Reader (Reader, ask)

import Data.ByteString            (ByteString)

import System.Posix               (Fd)
import System.Posix.IO.ByteString qualified as IO
import System.Posix.Terminal      qualified as Terminal
import System.Posix.Terminal      (TerminalAttributes, TerminalMode (..), TerminalState)
import System.Process             qualified as Process
import System.Process             (CreateProcess (..), ProcessHandle, StdStream (..))

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.TermSize        (TermSize, getTermSize, setTermSize)

installTerminalModes
  :: (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
installTerminalModes fd modes state = do
  termAttr <- sendM $ Terminal.getTerminalAttributes fd
  let newTermAttr = foldl Terminal.withMode termAttr modes

  logPty ["installTerminalModes"]
    $ printf "Fd: %s, TerminalAttributes: %s" (show fd) (showTerminalModes newTermAttr)

  sendM $ Terminal.setTerminalAttributes fd newTermAttr state

uninstallTerminalModes
  :: (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
uninstallTerminalModes fd modes state = do
  termAttr <- sendM $ Terminal.getTerminalAttributes fd
  let newTermAttr = foldl Terminal.withoutMode termAttr modes

  logPty ["uninstallTerminalModes"]
    $ printf "Fd: %s, TerminalAttributes: %s" (show fd) (showTerminalModes newTermAttr)

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


fdCloseIO :: (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs ()
fdCloseIO fd = logPty ["FdClose"] (printf "closing fd: %s" $ show fd) >> sendM (IO.closeFd fd)

forkAndExecCmdIO
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs)
  => Eff effs ProcessHandle
forkAndExecCmdIO = do
  Env { _envCmd, _envCmdArgs, _slavePty = (_, sHandle) } <- ask

  logPty ["ForkAndExecCmd"] $ printf "starting process by executing %s cmd" _envCmd

  (_, _, _, ph) <-
    sendM $ Process.createProcess_ "slave process" $ (Process.proc _envCmd _envCmdArgs)
      { delegate_ctlc = True
      , std_err       = UseHandle sHandle
      , std_out       = UseHandle sHandle
      , std_in        = UseHandle sHandle
      }
  return ph

getTerminalAttrIO
  :: (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> Eff effs TerminalAttributes
getTerminalAttrIO fd = do

  termAttr <- sendM (Terminal.getTerminalAttributes fd)

  logPty ["GetTerminalAttr"]
    $ printf "FD: %s, Terminal Attributes: %s" (show fd) (showTerminalModes termAttr)

  return termAttr

setTerminalAttrIO
  :: (Members '[Logger] effs, LastMember IO effs)
  => Fd
  -> TerminalAttributes
  -> TerminalState
  -> Eff effs ()
setTerminalAttrIO fd termAttr termState = do
  logPty ["SetTerminalAttr"] $ printf "Setting terminal attributes: %s, for fd: %d"
                                      (showTerminalModes termAttr)
                                      (toInteger fd)
  sendM (Terminal.setTerminalAttributes fd termAttr termState)

getTermSizeIO
  :: (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs TermSize
getTermSizeIO fd = do
  size <- sendM (getTermSize fd)
  logPty ["GetTermSize"] $ printf "FD: %s, TermSize: %s" (show fd) (show size)
  return size

setTermSizeIO
  ::(Members '[Logger] effs, LastMember IO effs) => Fd -> Fd -> Eff effs ()
setTermSizeIO fdFrom fdTo = do
  sendM (setTermSize fdFrom fdTo)
  newSize <- sendM $ getTermSize fdTo
  logPty ["SetTermSize"] $ printf "FD: %s, New TermSize: %s" (show fdTo) (show newSize)


writeStdout
  :: (Members '[Reader Env , HandleAct] effs) => ByteString -> Eff effs ()
writeStdout content = do
  stdout <- snd <$> getStdout
  hWrite stdout content

writeMaster
  :: (Members '[Reader Env , HandleAct] effs) => ByteString -> Eff effs ()
writeMaster content = do
  hmaster <- snd . _masterPty <$> ask
  hWrite hmaster content
