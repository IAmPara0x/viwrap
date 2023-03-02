module Viwrap.Logger
  ( LogContext (..)
  , Logger (..)
  , logInput
  , logOther
  , logOutput
  , logPoll
  , logPty
  , logVI
  ) where

import Control.Monad.Freer    (Eff, Member)
import Control.Monad.Freer.TH (makeEffect)


data LogContext
  = PollCtx
  | InputCtx
  | OutputCtx
  | VICtx
  | PtyCtx
  | OtherCtx
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Logger a where
  LogM :: LogContext -> [String] -> String -> Logger ()

makeEffect ''Logger

logPty :: Member Logger effs => [String] -> String -> Eff effs ()
logPty = logM PtyCtx . ("PTY" :)

logPoll :: Member Logger effs => [String] -> String -> Eff effs ()
logPoll = logM PollCtx . ("Poll" :)

logInput :: Member Logger effs => [String] -> String -> Eff effs ()
logInput = logM InputCtx . ("Input" :)

logOutput :: Member Logger effs => [String] -> String -> Eff effs ()
logOutput = logM OutputCtx . ("Output" :)

logVI :: Member Logger effs => [String] -> String -> Eff effs ()
logVI = logM VICtx . ("VI" :)

logOther :: Member Logger effs => [String] -> String -> Eff effs ()
logOther = logM OtherCtx . ("Other" :)
