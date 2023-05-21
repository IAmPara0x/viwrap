{-# LANGUAGE UndecidableInstances #-}
module Viwrap.Pty
  ( Child (..)
  , Env (..)
  , File (..)
  , HandleAct (..)
  , TermState (..)
  , Terminal (..)
  , Timeout (..)
  , ViStack
  , ViwrapHandle (..)
  , ViwrapHandles (..)
  , ViwrapStack
  , ViwrapState (..)
  , childStatus
  , currentPollRate
  , envBufferSize
  , envCmd
  , envCmdArgs
  , envPollingRate
  , fromViwrapHandle
  , getSignalReceived
  , getTermCursorPos
  , getTermSize
  , hStdErr
  , hStdIn
  , hStdOut
  , hWrite
  , isPromptUp
  , loggerConfig
  , masterPty
  , modifyViwrapHandle
  , onViwrapHandle
  , prevMasterContent
  , pselect
  , recievedCtrlD
  , signalReceived
  , slavePty
  , termCursorPos
  , termSize
  , viHooks
  , viState
  , viwrapHandles
  ) where

import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State)
import Control.Monad.Freer.TH     (makeEffect)

import Data.ByteString            (ByteString)
import Data.Default               (Default (def))
import Data.Functor.Identity      (Identity (..))
import Data.Kind                  (Type)
import Data.Sequence              (Seq)

import Lens.Micro.TH              (makeLenses)

import System.IO                  (Handle)
import System.Posix               (Fd)
import System.Posix.Signals       (Signal)

import Viwrap.Logger              (Logger, LoggerConfig)
import Viwrap.VI


data Timeout
  = Wait Int
  | Immediately
  | Infinite
  deriving stock (Show)

data File
  = FileHandle
  | FileContent
  deriving stock (Show)

type family ToFile (file :: File) :: Type where
  ToFile 'FileHandle = Handle
  ToFile 'FileContent = Maybe ByteString

data ViwrapHandle (f :: File)
  = StdIn (ToFile f)
  | StdOut (ToFile f)
  | StdErr (ToFile f)
  | MasterPty (ToFile f)
  | SlavePty (ToFile f)

deriving stock instance Show (ToFile f) => Show (ViwrapHandle f)


fromViwrapHandle :: forall f . ViwrapHandle f -> ToFile f
fromViwrapHandle = runIdentity . onViwrapHandle Identity

onViwrapHandle :: forall f f1 t1 . (ToFile f -> t1 f1) -> ViwrapHandle f -> t1 f1
onViwrapHandle f (StdIn     h) = f h
onViwrapHandle f (StdOut    h) = f h
onViwrapHandle f (StdErr    h) = f h
onViwrapHandle f (SlavePty  h) = f h
onViwrapHandle f (MasterPty h) = f h

modifyViwrapHandle :: forall f f1 . (ToFile f -> ToFile f1) -> ViwrapHandle f -> ViwrapHandle f1
modifyViwrapHandle f (StdIn     h) = StdIn $ f h
modifyViwrapHandle f (StdOut    h) = StdOut $ f h
modifyViwrapHandle f (StdErr    h) = StdErr $ f h
modifyViwrapHandle f (SlavePty  h) = SlavePty $ f h
modifyViwrapHandle f (MasterPty h) = MasterPty $ f h

data HandleAct a where
  HWrite :: ViwrapHandle 'FileHandle -> ByteString -> HandleAct ()
  Pselect :: [ViwrapHandle 'FileHandle] -> Timeout -> HandleAct [ViwrapHandle 'FileContent]

makeEffect ''HandleAct

data ViwrapHandles
  = ViwrapHandles
      { _masterPty :: (Fd, ViwrapHandle 'FileHandle)
      , _slavePty  :: (Fd, ViwrapHandle 'FileHandle)
      , _hStdIn    :: ViwrapHandle 'FileHandle
      , _hStdOut   :: ViwrapHandle 'FileHandle
      , _hStdErr   :: ViwrapHandle 'FileHandle
      }
  deriving stock (Show)

makeLenses ''ViwrapHandles

data Env
  = Env
      { _envCmd         :: String
      , _envCmdArgs     :: [String]
      , _envPollingRate :: Int
      , _envBufferSize  :: Int
      , _loggerConfig   :: LoggerConfig
      , _viwrapHandles  :: ViwrapHandles
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

type ViwrapStack = '[HandleAct , Terminal , State ViwrapState , Logger , Reader Env , IO]

type ViStack = '[State ViwrapState , Logger , Reader Env , IO]
