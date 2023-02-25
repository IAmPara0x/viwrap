module Viwrap.VI.Utils
  ( moveToBeginning
  , moveToEnd
  ) where


import Control.Monad              (void, when)
import Control.Monad.Freer        (Eff, Members, interpret)
import Control.Monad.Freer.Reader (Reader, asks)
import Control.Monad.Freer.State  (State, get, modify)

import Viwrap.Pty
import Viwrap.Pty.Utils
import Viwrap.VI

import Lens.Micro                 ((%~), (.~), (^.))

import Data.ByteString            (ByteString)
import Data.ByteString            qualified as BS
import Data.String                (fromString)

import System.Console.ANSI        qualified as ANSI


moveToBeginning :: ViwrapEff fd effs => Eff effs ()
moveToBeginning = do
  VILine {..} <- _viLine <$> get
  void $ moveLeft _viCursorPos

moveToEnd :: ViwrapEff fd effs => Eff effs ()
moveToEnd = do
  VILine {..} <- _viLine <$> get
  void $ moveRight (BS.length _viLineContents - _viCursorPos)
