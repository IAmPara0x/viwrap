module Viwrap.Signals
  ( passOnSignal
  , posixSignalHook
  , sigWINCHHandler
  ) where

import Control.Concurrent        (MVar, putMVar, takeMVar)
import Control.Monad.Freer       (Eff)
import Control.Monad.Freer.State (modify)

import Lens.Micro                ((.~))

import System.Posix              qualified as IO
import System.Posix              (Fd)
import System.Posix.Signals      qualified as Signals
import System.Posix.Signals      (Handler (Catch), Signal)
import System.Process            (Pid)

import Viwrap.Pty                hiding (Terminal (..), getTermSize)
import Viwrap.Pty.TermSize       (TermSize (..), getTermSize, setTermSize)
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
