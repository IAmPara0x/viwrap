module Capture.Viwrap.Pty.Handler
  ( CaptureContents (..)
  , UserInput (..)
  , initialCaptureContents
  , inputAt
  , inputContents
  , runHandleActTestIO
  , startTime
  , stdinContents
  , stdoutContents
  , captureTermSize
  ) where


import Control.Concurrent         (MVar, newMVar, putMVar, takeMVar)
import Control.Monad              (when, zipWithM_)
import Control.Monad.Freer        (Eff, LastMember, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State  (State, get)

import Data.Aeson                 (FromJSON, ToJSON)
import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Text                  (Text)
import Data.Text.Encoding         (decodeUtf8)
import Data.Time.Clock.POSIX      (getPOSIXTime)
import Data.Time                  (NominalDiffTime)
import Data.Maybe (fromMaybe)

import GHC.Generics               (Generic)

import Lens.Micro.TH              (makeLenses)

import System.Console.ANSI        qualified as ANSI
import System.IO                  (Handle, stderr, stdin, stdout)

import Text.Printf                (printf)

import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Handler         (pselectIO, hWriteIO)


data UserInput
  = UserInput
      { _inputContents :: Text
      , _inputAt       :: NominalDiffTime
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON UserInput
instance ToJSON UserInput

makeLenses ''UserInput

data CaptureContents
  = CaptureContents
      { _stdoutContents :: [Text]
      , _stdinContents  :: [UserInput]
      , _startTime      :: NominalDiffTime
      , _captureTermSize :: (Int,Int)
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON CaptureContents
instance ToJSON CaptureContents


initialCaptureContents :: IO (MVar CaptureContents)
initialCaptureContents = do
  _startTime <- getPOSIXTime
  _captureTermSize <- fromMaybe (error "Not able to get the terminal size: initialCaptureContents") <$> ANSI.hGetTerminalSize stdout
  newMVar $ CaptureContents { _startTime, _stdinContents = mempty, _stdoutContents = mempty, _captureTermSize }


makeLenses ''CaptureContents

runHandleActTestIO
  :: (Members '[Logger , Reader Env , State (MVar CaptureContents)] effs, LastMember IO effs)
  => Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActTestIO = interpret $ \case
  Pselect handles timeout -> pselectTestIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> pure stdin
  GetStdout               -> pure stdout
  GetStderr               -> pure stderr

pselectTestIO
  :: forall effs. (Members '[Logger , Reader Env , State (MVar CaptureContents)] effs, LastMember IO effs)
  => [Handle]
  -> Timeout
  -> Eff effs [Maybe ByteString]

-- TODO: This is very hacky as we assume that the first and second element of the list will be
-- master Handle and stdin handle respectively.
pselectTestIO [hMaster, hStdin] timeout = do

  res         <- pselectIO [hMaster, hStdin] timeout

  userInputAt <- sendM getPOSIXTime

  let captureStdIn :: Maybe ByteString -> Eff effs ()
      captureStdIn Nothing = pure ()
      captureStdIn (Just input) = do
        captureContentsMVar <- get

        captureContents     <- sendM $ takeMVar captureContentsMVar

        sendM $ putMVar
          captureContentsMVar
          captureContents
            { _stdinContents = _stdinContents captureContents
                                 <> [UserInput (decodeUtf8 input) userInputAt]
            }

      captureHMaster :: Maybe ByteString -> Eff effs ()
      captureHMaster Nothing = pure ()
      captureHMaster (Just content) = do

        captureContentsMVar <- get

        captureContents     <- sendM $ takeMVar captureContentsMVar

        sendM $ putMVar
          captureContentsMVar
          captureContents { _stdoutContents = _stdoutContents captureContents <> [decodeUtf8 content]
                          }

  zipWithM_ ($) [captureHMaster, captureStdIn] res
  pure res

pselectTestIO handles timeout = pselectIO handles timeout
