module Viwrap.VI.KeyMap
  ( defKeyMap
  , keyAction
  ) where


import Control.Monad            (void)
import Control.Monad.Freer      (Eff)

import Data.ByteString          qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.Map                 (Map)
import Data.Map                 qualified as Map
import Data.Word                (Word8)


import Viwrap.Pty
import Viwrap.VI
import Viwrap.VI.Handler


type KeyMap effs = Map (VIMode, Word8) (Eff ViwrapStack ())

defKeyMap :: KeyMap effs
defKeyMap = Map.mapKeys (fmap BS.c2w) $ Map.fromList
  [ ((Normal, '\n')  , handleNewline)
  , ((Normal, 'A')   , moveToEnd >> toInsertMode)
  , ((Normal, 'I')   , moveToBeginning >> toInsertMode)
  -- , ((Normal, 99), writeMaster @fd $ mconcat [BS.singleton 3])
  , ((Normal, 'd')   , void backspace)
  , ((Normal, 'h')   , void $ moveLeft 1)
  , ((Normal, 'i')   , toInsertMode)
  , ((Normal, 'l')   , void $ moveRight 1)
  , ((Normal, 'k')   , moveToPrevLine)
  , ((Normal, 'j')   , moveToNextLine)

       -- Insert Mode KeyMap
  , ((Insert, '\f'), insertNoUpdate $ BS.singleton $ BS.c2w '\f')
  , ((Insert, '\EOT'), insertNoUpdate $ BS.singleton $ BS.c2w '\EOT')
  , ((Insert, '\t')  , handleTab)
  , ((Insert, '\n')  , handleNewline)
  , ((Insert, '\ESC'), toNormalMode)
  , ((Insert, '\DEL'), backspace)
  ]


keyAction :: KeyMap ViwrapStack -> VIMode -> Word8 -> Eff ViwrapStack ()
keyAction keymap mode key = do
  case Map.lookup (mode, key) keymap of
    Just eff -> eff
    Nothing | mode == Insert -> void $ insertBS (BS.singleton key)
            | otherwise      -> pure ()
