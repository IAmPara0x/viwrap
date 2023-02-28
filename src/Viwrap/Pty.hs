{-# LANGUAGE UndecidableInstances #-}
module Viwrap.Pty
  ( Child (..)
  , Env (..)
  , HandleAct (..)
  , Process (..)
  , Timeout (..)
  , ToHandle
  , ViwrapEff
  , ViwrapState (..)
  , childStatus
  , currentPollRate
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
  , masterPty
  , prevMasterContent
  , pselect
  , slavePty
  , viHooks
  , viLine
  ) where

import Control.Monad.Freer        (Eff, Member, Members, send)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State)
import Control.Monad.Freer.TH     (makeEffect)

import Data.ByteString            (ByteString)
import Data.Sequence              (Seq)

import Lens.Micro.TH              (makeLenses)

import System.Exit                (ExitCode)
import System.IO                  (Handle)
import System.Posix               (Fd)
import System.Process             (ProcessHandle)

import Viwrap.Logger              (Logger)
import Viwrap.VI                  (VIEdit, VIHook, VILine, initialVILine)


data Timeout
  = Wait Int
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

data Process a where
  IsProcessDead :: ProcessHandle -> Process (Maybe ExitCode)

makeEffect ''Process


data Env fd
  = Env
      { _envCmd         :: String
      , _envCmdArgs     :: [String]
      , _envPollingRate :: Int
      , _envBufferSize  :: Int
      , _logFile        :: FilePath
      , _masterPty      :: (fd, ToHandle fd)
      , _slavePty       :: (fd, ToHandle fd)
      }

deriving stock instance (Show fd, Show (ToHandle fd)) => Show (Env fd)

makeLenses ''Env

data Child
  = Alive
  | Dead
  deriving stock (Eq, Show)

data ViwrapState
  = ViwrapState
      { _isPromptUp        :: Bool
      , _childStatus       :: Child
      , _prevMasterContent :: ByteString
      , _viLine            :: VILine
      , _viHooks           :: Seq VIHook
      , _currentPollRate   :: Int
      }
  deriving stock (Show)

makeLenses ''ViwrapState

type ViwrapEff fd effs
  = Members '[HandleAct fd , Logger , Process , Reader (Env fd) , State ViwrapState , VIEdit] effs

initialViwrapState :: ViwrapState
initialViwrapState = ViwrapState { _childStatus       = Alive
                                 , _isPromptUp        = False
                                 , _prevMasterContent = mempty
                                 , _viLine            = initialVILine
                                 , _viHooks           = mempty
                                 , _currentPollRate   = 10000
                                 }
