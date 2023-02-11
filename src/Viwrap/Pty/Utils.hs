module Viwrap.Pty.Utils
  ( installTerminalModes
  , uninstallTerminalModes
  ) where

import Control.Monad.Freer   (Eff, Members)
import System.Posix          (Fd)
import System.Posix.Terminal qualified as Terminal
import System.Posix.Terminal (TerminalMode, TerminalState)

import Viwrap.Pty

installTerminalModes
  :: forall effs
   . (Members '[Logger , PtyAct] effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
installTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldl Terminal.withMode termAttr modes

  setTerminalAttr fd newTermAttr state

uninstallTerminalModes
  :: forall effs
   . (Members '[Logger , PtyAct] effs)
  => Fd
  -> [TerminalMode]
  -> TerminalState
  -> Eff effs ()
uninstallTerminalModes fd modes state = do
  termAttr <- getTerminalAttr fd
  let newTermAttr = foldl Terminal.withoutMode termAttr modes

  setTerminalAttr fd newTermAttr state
