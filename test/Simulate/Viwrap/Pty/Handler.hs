module Simulate.Viwrap.Pty.Handler
  ( SimHandles (..)
  , feedStdIn
  , mkSimHandles
  , runHandleActSimIO
  ) where

import Control.Concurrent
import Control.Monad (zipWithM_)
import Control.Monad.Freer        (Eff, LastMember, Members, interpret)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State (State,modify,get)

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.Bifunctor (second,first)

import Data.Text (Text)
import Data.Text.Encoding         (encodeUtf8,decodeUtf8)

import System.IO                  qualified as IO
import System.IO                  (Handle)
import System.Process             qualified as Process

import Capture.Viwrap.Pty.Handler (UserInput (..))
import Data.Time                  (nominalDiffTimeToSeconds)
import Text.Printf                (printf)
import Viwrap.Logger
import Viwrap.Pty
import Viwrap.Pty.Handler         (hWriteIO, pselectIO)


data SimHandles
  = SimHandles
      { _simStdIn  :: (Handle, Handle)
      , _simStdOut :: Handle
      , _simStdErr :: Handle
      }

mkSimHandles :: IO SimHandles
mkSimHandles = do
  (readH, writeH) <- Process.createPipe
  hStdOut         <- snd <$> IO.openTempFile "/tmp/" "stdout.viwrap"
  hStdErr         <- snd <$> IO.openTempFile "/tmp/" "stderr.viwrap"

  IO.hSetBuffering readH IO.NoBuffering
  IO.hSetBuffering writeH IO.NoBuffering
  IO.hSetBuffering hStdOut IO.NoBuffering
  IO.hSetBuffering hStdErr IO.NoBuffering

  pure $ SimHandles { _simStdIn = (readH, writeH), _simStdOut = hStdOut, _simStdErr = hStdErr}

runHandleActSimIO
                                    -- HACK: This State is a hack, remove later 
  :: (Members '[Logger , Reader Env, State (ByteString,[Text])] effs, LastMember IO effs)
  => SimHandles
  -> Eff (HandleAct ': effs) a
  -> Eff effs a
runHandleActSimIO simH = interpret $ \case
  Pselect handles timeout -> pselectSimIO handles timeout
  HWrite  handle  content -> hWriteIO handle content
  GetStdin                -> pure $ fst $ _simStdIn simH
  GetStdout               -> pure $ _simStdOut simH
  GetStderr               -> pure $ _simStdErr simH


pselectSimIO
  :: forall effs. (Members '[Logger , Reader Env , State (ByteString,[Text])] effs, LastMember IO effs)
  => [Handle]
  -> Timeout
  -> Eff effs [Maybe ByteString]

-- HACK: This is very hacky as we assume that the first and second element of the list will be
-- master Handle and stdin handle respectively.
pselectSimIO [hMaster, hStdin] timeout = do

  res         <- pselectIO [hMaster, hStdin] timeout

  let captureHMaster :: Maybe ByteString -> Eff effs ()
      captureHMaster Nothing = pure ()
      captureHMaster (Just content) = do

        modify @(ByteString, [Text]) (second (<> [decodeUtf8 content]))

      handleInput :: Maybe ByteString -> Eff effs (Maybe ByteString)
      handleInput minput = do

        modify @(ByteString,[Text]) (first (\x -> mconcat [x,fromMaybe mempty minput]))

        inputs <- fst <$> get @(ByteString,[Text])

        if BS.take 1 inputs == mempty
          then pure Nothing
          else modify @(ByteString,[Text]) (first (BS.drop 1)) >> pure (Just $ BS.take 1 inputs)

  zipWithM_ ($) [captureHMaster] res

  newInput <- handleInput (res !! 1)

  x <- fst <$> get @(ByteString,[Text])
  logPoll ["pselectSimIO", "Input Buffer"] (show x)
  logPoll ["pselectSimIO", "New Poll Result"] (show [head res, newInput])

  pure [head res, newInput]

pselectSimIO handles timeout = pselectIO handles timeout

feedStdIn :: [UserInput] -> Int -> SimHandles -> IO ()
feedStdIn []                      _ _    = pure ()
feedStdIn (UserInput {..} : inputs) offset simH = do

  let simStdIn = snd $ _simStdIn simH
      content  = encodeUtf8 _inputContents
      delay    = ceiling (nominalDiffTimeToSeconds _inputAt * 1000000) + offset

  putStrLn (printf "Feeding input %s with a delay of %s" (show content) (show delay))

  threadDelay delay

  BS.hPutStr simStdIn content

  feedStdIn inputs offset simH
