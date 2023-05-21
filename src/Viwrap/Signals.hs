module Viwrap.Signals
  ( passOnSignal
  , posixSignalHook
  , sigCHLDHandler
  , sigWINCHHandler
  ) where

import Control.Concurrent         (MVar, putMVar, takeMVar)
import Control.Monad.Freer        (Eff, Members, runM)
import Control.Monad.Freer.Reader (Reader, runReader)
import Control.Monad.Freer.State  (modify)

import Data.ByteString            (ByteString)

import Lens.Micro                 ((&), (.~))

import System.Exit                (ExitCode (..))
import System.Posix               qualified as IO
import System.Posix               (Fd)
import System.Posix.Signals       qualified as Signals
import System.Posix.Signals       (Handler (Catch), Signal)
import System.Process             (Pid)

import Viwrap.Logger
import Viwrap.Logger.Handler      (runLoggerUnit)
import Viwrap.Pty                 hiding (Terminal (..), getTermSize)
import Viwrap.Pty.Handler         (runHandleActIO)
import Viwrap.Pty.TermSize        (TermSize (..), getTermSize, setTermSize)
import Viwrap.Pty.Utils           (getMasterPty, writeStdout)
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

posixSignalHook :: Eff ViwrapStack ()
posixSignalHook = do
  msignal <- signalReceived

  case msignal of
    Nothing -> pure ()
    (Just signal) | signal == Signals.sigINT -> handleSigInt
                  | otherwise                -> pure ()


handleSigInt :: Eff ViwrapStack ()
handleSigInt = do
  modify (viState . currentLine .~ mempty)

sigCHLDHandler :: Env -> IO () -> Handler
sigCHLDHandler env cleanup = Catch do

  runHandleActIO handleDeadChild & runLoggerUnit & runReader env & runM

  cleanup

  IO.exitImmediately ExitSuccess

 where

  handleDeadChild :: forall effs . Members '[HandleAct , Logger , Reader Env] effs => Eff effs ()
  handleDeadChild = do

    hmaster <- getMasterPty
    result  <- pselect [hmaster] Immediately

    let handleFileContent :: Maybe ByteString -> Eff effs ()
        handleFileContent Nothing =
          logOther ["childDied"] "Read all the output from slave process, exiting now."
        handleFileContent (Just content) = writeStdout content >> handleDeadChild

    mapM_ (onViwrapHandle handleFileContent) result
