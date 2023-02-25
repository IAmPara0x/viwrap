{-# LANGUAGE UndecidableInstances #-}
module Viwrap.Pty
  ( Cmd
  , Env (..)
  , HandleAct (..)
  , Logger (..)
  , Process (..)
  , Timeout (..)
  , ToHandle
  , ViwrapEff
  , ViwrapState (..)
  , childIsDead
  , envBufferSize
  , envCmd
  , envCmdArgs
  , envPollingRate
  , getStderr
  , getStdin
  , getStdout
  , hWrite
  , initialViwrapState
  , isProcessDead
  , isPromptUp
  , logFile
  , logM
  , masterPty
  , prevMasterContent
  , pselect
  , setCursorPos
  , slavePty
  , viLine
  ) where

import Control.Monad.Freer        (Eff, Member, Members, send)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State)
import Control.Monad.Freer.TH     (makeEffect)
import Data.ByteString            (ByteString)
import Lens.Micro.TH              (makeLenses)
import System.Exit                (ExitCode)
import System.IO                  (Handle)
import System.Posix               (Fd)
import System.Process             (ProcessHandle)
import Viwrap.VI                  (VIEdit, VILine, initialVILine)


type Cmd = String

data Timeout
  = Wait
  | Immediately
  | Infinite
  deriving stock (Show)

type family ToHandle a
type instance ToHandle Fd = Handle

data HandleAct fd a where
  Pselect :: [ToHandle fd] -> Timeout -> HandleAct fd [Maybe ByteString]
  HWrite :: ToHandle fd -> ByteString -> HandleAct fd ()
  GetStdin :: HandleAct fd (fd, ToHandle fd)
  GetStdout :: HandleAct fd (fd, ToHandle fd)
  GetStderr :: HandleAct fd (fd, ToHandle fd)


pselect
  :: forall fd effs
   . (Member (HandleAct fd) effs)
  => [ToHandle fd]
  -> Timeout
  -> Eff effs [Maybe ByteString]
pselect handles = send . Pselect @fd handles

hWrite
  :: forall fd effs . (Member (HandleAct fd) effs) => ToHandle fd -> ByteString -> Eff effs ()
hWrite handle = send . HWrite @fd handle

getStdin :: forall fd effs . (Member (HandleAct fd) effs) => Eff effs (fd, ToHandle fd)
getStdin = send GetStdin

getStdout :: forall fd effs . (Member (HandleAct fd) effs) => Eff effs (fd, ToHandle fd)
getStdout = send GetStdout

getStderr :: forall fd effs . (Member (HandleAct fd) effs) => Eff effs (fd, ToHandle fd)
getStderr = send GetStderr

data Logger a where
  LogM :: String -> [String] -> Logger ()

makeEffect ''Logger

data Process a where
  IsProcessDead :: ProcessHandle -> Process (Maybe ExitCode)

makeEffect ''Process

data Env fd
  = Env
      { _envCmd         :: Cmd
      , _envCmdArgs     :: [String]
      , _envPollingRate :: Int
      , _envBufferSize  :: Int
      , _logFile        :: FilePath
      , _masterPty      :: (fd, ToHandle fd)
      , _slavePty       :: (fd, ToHandle fd)
      }

deriving stock instance (Show fd, Show (ToHandle fd)) => Show (Env fd)

makeLenses ''Env

data ViwrapState
  = ViwrapState
      { _isPromptUp        :: Bool
      , _setCursorPos      :: Bool
      , _childIsDead       :: Bool
      , _prevMasterContent :: ByteString
      , _viLine            :: VILine
      }
  deriving stock (Show)

makeLenses ''ViwrapState

type ViwrapEff fd effs
  = Members '[HandleAct fd , Logger , Process , Reader (Env fd) , State ViwrapState , VIEdit] effs


initialViwrapState :: ViwrapState
initialViwrapState = ViwrapState { _childIsDead       = False
                                 , _isPromptUp        = False
                                 , _prevMasterContent = mempty
                                 , _viLine            = initialVILine
                                 , _setCursorPos      = False
                                 }
