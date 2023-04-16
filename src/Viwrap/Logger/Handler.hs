module Viwrap.Logger.Handler
  ( runLoggerIO
  , runLoggerUnit
  ) where

import Control.Monad              (when)
import Control.Monad.Freer        (Eff, LastMember, Members, interpret, sendM)
import Control.Monad.Freer.Reader (Reader, ask)
import Text.Printf                (printf)
import Viwrap.Logger
import Viwrap.Pty


-- Handlers for logger
runLoggerUnit :: Eff (Logger ': effs) a -> Eff effs a
runLoggerUnit = interpret $ \case
  LogM{} -> return ()

runLoggerIO
  :: (Members '[Reader Env] effs, LastMember IO effs)
  => [LogContext]
  -> Eff (Logger ': effs) a
  -> Eff effs a
runLoggerIO ctxs = interpret $ \case
  LogM ctx args str -> when
    (ctx `elem` ctxs)
    do
      Env { _logFile } <- ask
      sendM $ appendFile _logFile $ logStr args str

logStr :: [String] -> String -> String
logStr []       str = str <> "\n"
logStr [x     ] str = printf "[%s] %s\n" x str
logStr (x : xs) str = printf "[%s]" x <> logStr xs str
