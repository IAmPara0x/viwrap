module Viwrap.Logger
  ( LogContext (..)
  , Logger (..)
  , LoggerConfig (..)
  , logCtxs
  , logError
  , logFile
  , logInput
  , logOther
  , logOutput
  , logPoll
  , logPty
  , logVI
  ) where

import Control.Monad.Freer    (Eff, Member)
import Control.Monad.Freer.TH (makeEffect)

import Lens.Micro.TH          (makeLenses)

data LogContext
  = PollCtx
  | InputCtx
  | OutputCtx
  | VICtx
  | PtyCtx
  | OtherCtx
  | ErrorCtx
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Logger a where
  LogM :: LogContext -> [String] -> String -> Logger ()

makeEffect ''Logger

data LoggerConfig
  = LoggerConfig
      { _logFile :: FilePath
      , _logCtxs :: [LogContext]
      }
  deriving stock (Show)

makeLenses ''LoggerConfig

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

logError :: Member Logger effs => [String] -> String -> Eff effs ()
logError = logM ErrorCtx . ("Error" :)
