{-# LANGUAGE UndecidableInstances #-}
module Viwrap.Pty
  ( Child (..)
  , Env (..)
  , HandleAct (..)
  , Process (..)
  , Timeout (..)
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
  , hCursorPos
  , hTerminalSize
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

import Control.Monad.Freer        (Members)
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

data HandleAct a where
  Pselect :: [Handle] -> Timeout -> HandleAct [Maybe ByteString]
  HWrite :: Handle -> ByteString -> HandleAct ()
  GetStdin :: HandleAct (Fd, Handle)
  GetStdout :: HandleAct (Fd, Handle)
  GetStderr :: HandleAct (Fd, Handle)
  HCursorPos :: Handle -> HandleAct (Maybe (Int, Int))
  HTerminalSize :: Handle -> HandleAct (Maybe (Int, Int))

makeEffect ''HandleAct

data Process a where
  IsProcessDead :: ProcessHandle -> Process (Maybe ExitCode)

makeEffect ''Process

data Env
  = Env
      { _envCmd         :: String
      , _envCmdArgs     :: [String]
      , _envPollingRate :: Int
      , _envBufferSize  :: Int
      , _logFile        :: FilePath
      , _masterPty      :: (Fd, Handle)
      , _slavePty       :: (Fd, Handle)
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
      , _prevMasterContent :: ByteString
      , _viLine            :: VILine
      , _viHooks           :: Seq VIHook
      , _currentPollRate   :: Int
      }
  deriving stock (Show)

makeLenses ''ViwrapState

type ViwrapEff effs
  = Members '[HandleAct , Logger , Process , Reader Env , State ViwrapState , VIEdit] effs

initialViwrapState :: ViwrapState
initialViwrapState = ViwrapState { _childStatus       = Alive
                                 , _isPromptUp        = False
                                 , _prevMasterContent = mempty
                                 , _viLine            = initialVILine
                                 , _viHooks           = mempty
                                 , _currentPollRate   = 10000
                                 }
