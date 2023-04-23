module Capture.Viwrap.Pty.Handler
  ( CaptureContents (..)
  , stdoutContents
  , stdinContents
  , startTime
  , inputContents
  , inputAt
  , UserInput(..)
  , runHandleActTestIO
  , initialCaptureContents
  ) where


import Control.Concurrent         (MVar, newMVar, takeMVar, putMVar)
import Control.Monad.Freer        (Eff, LastMember, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State, get)

import Data.Aeson (FromJSON,ToJSON)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Time (NominalDiffTime)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import GHC.Generics (Generic)

import Lens.Micro.TH              (makeLenses)

import System.IO                  (Handle, stderr, stdin, stdout)

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Handler         (pselectIO)
import Data.Time.Clock.POSIX      (getPOSIXTime)


data UserInput = UserInput
  { _inputContents :: Text
  , _inputAt :: NominalDiffTime
  } deriving stock (Eq, Generic, Show)

instance FromJSON UserInput
instance ToJSON UserInput

makeLenses ''UserInput

data CaptureContents
  = CaptureContents
      { _stdoutContents :: [Text]
      , _stdinContents  :: [UserInput]
      , _startTime :: NominalDiffTime
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON CaptureContents
instance ToJSON CaptureContents


initialCaptureContents :: IO (MVar CaptureContents)
initialCaptureContents = do
  _startTime <- getPOSIXTime
  newMVar $ CaptureContents
    { _startTime
    , _stdinContents=mempty
    , _stdoutContents=mempty
    }

  
makeLenses ''CaptureContents

runHandleActTestIO
  :: (Members '[Logger , Reader Env , State (MVar CaptureContents)] effs, LastMember IO effs)
  => Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActTestIO = interpret $ \case
  Pselect handles timeout -> pselectTestIO handles timeout
  HWrite  handle  content -> hWriteTestIO handle content
  GetStdin                -> pure stdin
  GetStdout               -> pure stdout
  GetStderr               -> pure stderr

hWriteTestIO
  :: (Members '[Logger , State (MVar CaptureContents)] effs, LastMember IO effs)
  => Handle
  -> ByteString
  -> Eff effs ()
hWriteTestIO handle content = do
  logOutput ["FdWrite"] $ printf "writing %s to %s" (show content) (show handle)
  sendM (BS.hPutStr handle content)

  captureContentsMVar <- get

  captureContents <- sendM $ takeMVar captureContentsMVar 

  sendM $ putMVar captureContentsMVar  captureContents
    {_stdoutContents = _stdoutContents captureContents <> [decodeUtf8 content]}

pselectTestIO
  :: (Members '[Logger , Reader Env , State (MVar CaptureContents)] effs, LastMember IO effs)
  => [Handle]
  -> Timeout
  -> Eff effs [Maybe ByteString]
pselectTestIO [hMaster, hStdin] timeout = do

  res <- pselectIO [hMaster, hStdin] timeout

  userInputAt <- sendM getPOSIXTime


  case res of
    [_, Just input] -> do

      captureContentsMVar <- get

      captureContents <- sendM $ takeMVar captureContentsMVar 

      sendM $ putMVar captureContentsMVar  captureContents
        {_stdinContents = _stdinContents captureContents <> [UserInput (decodeUtf8 input) userInputAt]}

    _               -> pure ()
  pure res

pselectTestIO handles timeout = pselectIO handles timeout
