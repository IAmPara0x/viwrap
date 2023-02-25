module Viwrap.VI.Utils
  ( KeyMap
  , defKeyMap
  , keyAction
  , moveToBeginning
  , moveToEnd
  ) where


import Control.Monad             (void)
import Control.Monad.Freer       (Eff)
import Control.Monad.Freer.State (get, modify)

import Data.Map                  (Map)
import Data.Map                  qualified as Map
import Data.Word                 (Word8)
import Lens.Micro
import Viwrap.Pty
import Viwrap.Pty.Utils
import Viwrap.VI

import Data.ByteString           qualified as BS

moveToBeginning :: ViwrapEff fd effs => Eff effs ()
moveToBeginning = do
  VILine {..} <- _viLine <$> get
  void $ moveLeft _viCursorPos

moveToEnd :: ViwrapEff fd effs => Eff effs ()
moveToEnd = do
  VILine {..} <- _viLine <$> get
  void $ moveRight (BS.length _viLineContent - _viCursorPos)


handleNewline :: forall fd effs . (ViwrapEff fd effs) => Eff effs ()
handleNewline = do
  writeMaster @fd "\n"
  modify (viLine .~ initialVILine)
  modify (isPromptUp .~ False)
  modify (setCursorPos .~ True)

toMode :: forall fd effs . (ViwrapEff fd effs) => VIMode -> Eff effs ()
toMode mode = modify (viLine . viMode .~ mode)

type KeyMap fd effs = ViwrapEff fd effs => Map (VIMode, Word8) (Eff effs ())

defKeyMap :: forall fd effs . KeyMap fd effs
defKeyMap = Map.fromList
  [ ((Normal, 10) , handleNewline @fd)
  , ((Normal, 65) , moveToEnd @fd >> toMode @fd Insert)
  , ((Normal, 73) , moveToBeginning @fd >> toMode @fd Insert)
  , ((Normal, 100), void backspace)
  , ((Normal, 104), void $ moveLeft 1)
  , ((Normal, 105), modify (viLine . viMode .~ Insert))
  , ((Normal, 108), void $ moveRight 1)

       -- Insert Mode KeyMap
  , ((Insert, 10) , handleNewline @fd)
  , ((Insert, 27) , modify (viLine . viMode .~ Normal))
  , ((Insert, 127), void backspace)
  ]

keyAction
  :: forall fd effs . (ViwrapEff fd effs) => KeyMap fd effs -> VIMode -> Word8 -> Eff effs ()
keyAction keymap mode key = do
  case Map.lookup (mode, key) keymap of
    Just eff -> eff
    Nothing | mode == Insert -> void $ insertBS (BS.singleton key)
            | otherwise      -> return ()
