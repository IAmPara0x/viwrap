module Viwrap.Signals
  ( passOnSignal
  , posixSignalHook
  , sigCHLDHandler
  , sigWINCHHandler
  , handleDeadChild
  ) where

import Control.Concurrent         (MVar, putMVar, takeMVar)
import Control.Monad.Freer        (Eff, LastMember, Members, runM)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State  (modify)

import Lens.Micro                 ((&), (.~))

import System.Exit                (ExitCode (..))
import System.Posix               qualified as IO
import System.Posix               (Fd)
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals       (Handler (Catch), Signal)
import System.Process             (Pid)

import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerIO, runLoggerUnit)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runHandleActIO)
import Viwrap.Pty.TermSize        (TermSize (..), getTermSize, setTermSize)
import Viwrap.Pty.Utils           (writeStdout)
import Viwrap.VI


passOnSignal :: Pid -> Signal -> MVar TermState -> Handler
passOnSignal pid signal termStateMVar = Catch do
  Signals.signalProcess signal pid
  termState <- takeMVar termStateMVar
  putMVar termStateMVar $ termState { _getSignalReceived = Just signal }


sigWINCHHandler :: MVar TermState -> Fd -> Handler
sigWINCHHandler termStateMVar fdMaster = Catch do
  setTermSize IO.stdInput fdMaster
  termState     <- takeMVar termStateMVar
  TermSize {..} <- getTermSize IO.stdInput
  putMVar termStateMVar (termState { _getTermSize = Just (termHeight, termWidth) })

posixSignalHook :: ViwrapEff effs => Eff effs ()
posixSignalHook = do
  msignal <- signalReceived

  case msignal of
    Nothing -> pure ()
    (Just signal) | signal == Signals.sigINT -> handleSigInt
                  | otherwise                -> pure ()


handleSigInt :: ViwrapEff effs => Eff effs ()
handleSigInt = do
  modify (viState . currentLine .~ mempty)

sigCHLDHandler :: Env -> IO () -> Handler
sigCHLDHandler env cleanup = Catch do

  runHandleActIO handleDeadChild & runLoggerUnit & runReader env & runM

  cleanup

  IO.exitImmediately ExitSuccess


handleDeadChild
  :: (Members '[Logger , Reader Env , HandleAct] effs, LastMember IO effs) => Eff effs ()
handleDeadChild = do

  hmaster <- snd . _masterPty <$> ask
  result  <- pselect [hmaster] Immediately

  let [mMasterContent] = result

  case mMasterContent of
    Nothing      -> logOther ["childDied"] "Read all the output from slave process, exiting now."
    Just content -> writeStdout content >> handleDeadChild
