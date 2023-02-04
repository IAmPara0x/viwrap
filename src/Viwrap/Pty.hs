module Viwrap.Pty ( Cmd
                  , Env(..)
                  , FdMaster
                  , FdSlave
                  , HMaster
                  , HSlave
                  , Logger(..)
                  , PtyAct(..)
                  , logM
                  , logS
                  , openPty
                  , fdClose
                  , hRead
                  , hWrite
                  , fdToHandle
                  , forkAndExecCmd
                  , getTerminalAttr
                  , setTerminalAttr
                  ) where

import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString (ByteString)
import System.Process (ProcessHandle)
import System.Posix (Fd)
import System.Posix.Terminal (TerminalAttributes, TerminalState)
import System.IO (Handle)

type HMaster = Handle
type HSlave = Handle

type FdMaster = Fd
type FdSlave = Fd

type Cmd = String

-- TODO: Is there a way to unite the API to use either Fd or Handle insread of using both?
data PtyAct a where
     OpenPty :: PtyAct (FdMaster, FdSlave)
     FdClose :: Fd -> PtyAct ()
     HRead :: Handle -> PtyAct (Maybe ByteString)
     HWrite :: Handle -> ByteString -> PtyAct ()
     FdToHandle :: Fd -> PtyAct Handle
     ForkAndExecCmd :: HSlave -> PtyAct ProcessHandle
     GetTerminalAttr :: Fd -> PtyAct TerminalAttributes
     SetTerminalAttr :: Fd -> TerminalAttributes -> TerminalState -> PtyAct ()
makeEffect ''PtyAct

data Logger a where
     LogM :: String -> [String] -> Logger ()

makeEffect ''Logger

logS :: forall a b effs. (Show a, Show b, Member Logger effs) => a -> [b] -> Eff effs ()
logS a = send . LogM (show a) . map show

data Env = Env { envCmd :: Cmd
               , envCmdArgs :: [String]
               , envPollingRate :: Int
               , envBufferSize :: Int
               , logFile :: FilePath
               }
