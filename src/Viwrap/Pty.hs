module Viwrap.Pty
  ( Cmd
  , Env (..)
  , FdMaster
  , FdSlave
  , HMaster
  , HSlave
  , HandleAct (..)
  , Logger (..)
  , PtyAct (..)
  , Terminal (..)
  , fdClose
  , fdToHandle
  , forkAndExecCmd
  , getStderr
  , getStdin
  , getStdout
  , getTerminalAttr
  , hRead
  , hWrite
  , logM
  , openPty
  , setTerminalAttr
  ) where

import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString        (ByteString)
import System.IO              (Handle)
import System.Posix           (Fd)
import System.Posix.Terminal  (TerminalAttributes, TerminalState)
import System.Process         (ProcessHandle)

type HMaster = Handle
type HSlave = Handle

type FdMaster = Fd
type FdSlave = Fd

type Cmd = String

-- TODO: Is there a way to unite the API to use either Fd or Handle insread of using both?
data PtyAct a where
  OpenPty :: PtyAct (FdMaster, FdSlave)
  FdToHandle :: Fd -> PtyAct Handle
  ForkAndExecCmd :: HSlave -> PtyAct ProcessHandle
  FdClose :: Fd -> PtyAct ()
  GetTerminalAttr :: Fd -> PtyAct TerminalAttributes
  SetTerminalAttr :: Fd -> TerminalAttributes -> TerminalState -> PtyAct ()

makeEffect ''PtyAct

data HandleAct a where
  HRead :: Handle -> HandleAct (Maybe ByteString)
  HWrite :: Handle -> ByteString -> HandleAct ()

makeEffect ''HandleAct

data Terminal a where
  GetStdin :: Terminal (Fd, Handle)
  GetStdout :: Terminal (Fd, Handle)
  GetStderr :: Terminal (Fd, Handle)

makeEffect ''Terminal

data Logger a where
  LogM :: String -> [String] -> Logger ()

makeEffect ''Logger

data Env
  = Env
      { envCmd         :: Cmd
      , envCmdArgs     :: [String]
      , envPollingRate :: Int
      , envBufferSize  :: Int
      , logFile        :: FilePath
      }
