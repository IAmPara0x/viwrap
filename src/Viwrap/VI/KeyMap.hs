module Viwrap.VI.KeyMap
  ( defKeyMap
  , keyAction
  ) where


import Control.Monad             (void)
import Control.Monad.Freer       (Eff)
import Control.Monad.Freer.State (modify)

import Data.ByteString           qualified as BS
import Data.Map                  (Map)
import Data.Map                  qualified as Map
import Data.Word                 (Word8)

import Lens.Micro                ((.~))

import Viwrap.Pty
import Viwrap.VI
import Viwrap.VI.Handler         (handleNewline, handleTab)
import Viwrap.VI.Utils           (moveToBeginning, moveToEnd, moveToNextWord, toMode)


type KeyMap fd effs = ViwrapEff fd effs => Map (VIMode, Word8) (Eff effs ())

defKeyMap :: forall fd effs . KeyMap fd effs
defKeyMap = Map.fromList
  [ ((Normal, 10) , handleNewline @fd)
  , ((Normal, 65) , moveToEnd @fd >> toMode @fd Insert)
  , ((Normal, 73) , moveToBeginning @fd >> toMode @fd Insert)
  -- , ((Normal, 99), writeMaster @fd $ mconcat [BS.singleton 3])
  , ((Normal, 100), void backspace)
  , ((Normal, 104), void $ moveLeft 1)
  , ((Normal, 105), modify (viLine . viMode .~ Insert))
  , ((Normal, 108), void $ moveRight 1)
  , ((Normal, 119), moveToNextWord @fd)

       -- Insert Mode KeyMap
  , ((Insert, 9)  , handleTab @fd)
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
