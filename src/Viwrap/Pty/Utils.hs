module Viwrap.Pty.Utils
  ( fdCloseIO
  , forkAndExecCmdIO
  , getTerminalAttrIO
  , hGetCursorPosition
  , installTerminalModes
  , setTerminalAttrIO
  , uninstallTerminalModes
  , writeMaster
  , writeStdout
  ) where

import Control.Monad                (when)
import Control.Monad.Freer          (Eff, LastMember, Members, sendM)
import Control.Monad.Freer.Reader   (Reader, ask)

import Data.ByteString              (ByteString)
import Data.Maybe                   (fromMaybe)

import System.IO                    qualified as IO
import System.IO                    (Handle)
import System.Posix                 (Fd)
import System.Posix.IO.ByteString   qualified as IO
import System.Posix.Terminal        qualified as Terminal
import System.Posix.Terminal        (TerminalAttributes, TerminalMode (..), TerminalState)
import System.Process               qualified as Process
import System.Process               (CreateProcess (..), Pid, ProcessHandle, StdStream (..))

import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Printf                  (printf)

import System.Console.ANSI          qualified as ANSI
import Viwrap.Logger
import Viwrap.Pty                   hiding (getTermSize)

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
  :: (Members '[Logger , Reader Env] effs, LastMember IO effs) => Eff effs (ProcessHandle, Pid)
forkAndExecCmdIO = do
  Env { _envCmd, _envCmdArgs, _slavePty = (_, sHandle) } <- ask

  logPty ["ForkAndExecCmd"] $ printf "starting process by executing %s cmd" _envCmd

  (_, _, _, ph) <-
    sendM $ Process.createProcess_ "slave process" $ (Process.proc _envCmd _envCmdArgs)
      { delegate_ctlc = False
      , std_err       = UseHandle sHandle
      , std_out       = UseHandle sHandle
      , std_in        = UseHandle sHandle
      , new_session   = True
      }

  pid <- fromMaybe (error "ERROR: the slave process got closed") <$> sendM (Process.getPid ph)
  pure (ph, pid)

getTerminalAttrIO
  :: (Members '[Logger] effs, LastMember IO effs) => Fd -> Eff effs TerminalAttributes
getTerminalAttrIO fd = do

  termAttr <- sendM (Terminal.getTerminalAttributes fd)

  logPty ["GetTerminalAttr"]
    $ printf "FD: %s, Terminal Attributes: %s" (show fd) (showTerminalModes termAttr)

  pure termAttr

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


writeStdout :: (Members '[Reader Env , HandleAct] effs) => ByteString -> Eff effs ()
writeStdout content = do
  stdout <- getStdout
  hWrite stdout content

writeMaster :: (Members '[Reader Env , HandleAct] effs) => ByteString -> Eff effs ()
writeMaster content = do
  hmaster <- snd . _masterPty <$> ask
  hWrite hmaster content

hGetCursorPosition
  :: forall effs
   . (Members '[Logger] effs, LastMember IO effs)
  => Handle
  -> Eff effs (Maybe (Int, Int))
hGetCursorPosition h = do
  sendM clearStdin
  sendM $ ANSI.hReportCursorPosition h
  fmap to0base <$> getCursorPosition' mempty
 where

  to0base :: (Int, Int) -> (Int, Int)
  to0base (row, col) = (row - 1, col - 1)

-- TODO: inputBuffer is the buffer of the user inputs (generally happens when user is typing blazingly fast)
-- that we read instead of reading cursorPosition. Currently, we don't do anything with this buffer but the
-- goal is to add this inputBuffer back to input and process it somehow
  getCursorPosition' :: String -> Eff effs (Maybe (Int, Int))
  getCursorPosition' inputBuffer = do

    cursorPosStr <- sendM ANSI.getReportedCursorPosition

    logOutput ["hGetCursorPosition", "inputBuffer"] $ "Input Buffer content: " <> inputBuffer
    logOutput ["hGetCursorPosition"] (show cursorPosStr)

    case readP_to_S ANSI.cursorPosition cursorPosStr of
      []                -> getCursorPosition' (inputBuffer <> cursorPosStr)
      [((row, col), _)] -> pure $ Just (row, col)
      (_ : _          ) -> pure Nothing

  clearStdin :: IO ()
  clearStdin = do
    isReady <- IO.hReady IO.stdin
    when isReady $ do
      _ <- getChar
      clearStdin
