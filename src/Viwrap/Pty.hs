{-# LANGUAGE UndecidableInstances #-}
module Viwrap.Pty
  ( Child (..)
  , Env (..)
  , HandleAct (..)
  , TermState (..)
  , Terminal (..)
  , Timeout (..)
  , ViwrapEff
  , ViwrapState (..)
  , childStatus
  , currentPollRate
  , envBufferSize
  , envCmd
  , envCmdArgs
  , envPollingRate
  , getSignalReceived
  , getStderr
  , getStdin
  , getStdout
  , getTermCursorPos
  , getTermSize
  , hWrite
  , isProcessDead
  , isPromptUp
  , logFile
  , masterPty
  , prevMasterContent
  , pselect
  , recievedCtrlD
  , signalReceived
  , slavePid
  , slavePty
  , termCursorPos
  , termSize
  , viHooks
  , viState
  ) where

import Control.Monad.Freer        (Members)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State)
import Control.Monad.Freer.TH     (makeEffect)

import Data.ByteString            (ByteString)
import Data.Default               (Default (def))
import Data.Sequence              (Seq)

import Lens.Micro.TH              (makeLenses)

import System.Exit                (ExitCode)
import System.IO                  (Handle)
import System.Posix               (Fd)
import System.Posix.Signals       (Signal)
import System.Process             (Pid, ProcessHandle)

import Viwrap.Logger              (Logger)
import Viwrap.VI


data Timeout
  = Wait Int
  | Immediately
  | Infinite
  deriving stock (Show)

data HandleAct a where
  GetStderr :: HandleAct (Fd, Handle)
  GetStdin :: HandleAct (Fd, Handle)
  GetStdout :: HandleAct (Fd, Handle)
  HWrite :: Handle -> ByteString -> HandleAct ()
  Pselect :: [Handle] -> Timeout -> HandleAct [Maybe ByteString]
  IsProcessDead :: ProcessHandle -> HandleAct (Maybe ExitCode)

makeEffect ''HandleAct

data Env
  = Env
      { _envCmd         :: String
      , _envCmdArgs     :: [String]
      , _envPollingRate :: Int
      , _envBufferSize  :: Int
      , _logFile        :: FilePath
      , _masterPty      :: (Fd, Handle)
      , _slavePty       :: (Fd, Handle)
      , _slavePid       :: Pid
      }
  deriving stock (Show)

makeLenses ''Env

data Child
  = Alive
  | Dead
  deriving stock (Eq, Show)

data ViwrapState
  = ViwrapState
      { _isPromptUp        :: Bool
      , _childStatus       :: Child
      , _recievedCtrlD     :: Bool
      , _prevMasterContent :: ByteString
      , _viHooks           :: Seq VIHook
      , _currentPollRate   :: Int
      , _viState           :: VIState
      }
  deriving stock (Show)

makeLenses ''ViwrapState

instance Default ViwrapState where
  def = ViwrapState { _childStatus       = Alive
                    , _isPromptUp        = False
                    , _prevMasterContent = mempty
                    , _viHooks           = mempty
                    , _currentPollRate   = 10000
                    , _viState           = def
                    , _recievedCtrlD     = False
                    }

data Terminal a where
  TermSize :: Terminal (Int, Int)
  TermCursorPos :: Terminal (Int, Int)
  SignalReceived :: Terminal (Maybe Signal)

makeEffect ''Terminal

data TermState
  = TermState
      { _getTermSize       :: Maybe (Int, Int)
      , _getTermCursorPos  :: Maybe (Int, Int)
      , _getSignalReceived :: Maybe Signal
      }
  deriving stock (Eq, Show)

makeLenses ''TermState

type ViwrapEff effs
  = Members '[HandleAct , Logger , Reader Env , State ViwrapState , Terminal] effs
