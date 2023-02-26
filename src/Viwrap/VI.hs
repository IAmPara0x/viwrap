module Viwrap.VI
  ( VIEdit (..)
  , VIHook (..)
  , VILine (..)
  , VIMode (..)
  , backspace
  , initialVILine
  , insertBS
  , moveLeft
  , moveRight
  , updateVILine
  , viCursorPos
  , viLineContent
  , viMode
  ) where

import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString        (ByteString)
import Data.ByteString        qualified as BS
import Lens.Micro.TH          (makeLenses)

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Eq, Ord, Show)

data VILine
  = VILine
      { _viLineContent :: ByteString
      , _viCursorPos   :: Int
      , _viMode        :: VIMode
      }
  deriving stock (Eq, Show)

makeLenses ''VILine

data VIEdit a where
  Backspace :: VIEdit VILine
  MoveLeft :: Int -> VIEdit VILine
  MoveRight :: Int -> VIEdit VILine
  InsertBS :: ByteString -> VIEdit VILine

makeEffect ''VIEdit

data VIHook
  = SyncCursor
  | TabPressed
  deriving stock (Eq, Show)

initialVILine :: VILine
initialVILine = VILine { _viLineContent = mempty, _viCursorPos = 0, _viMode = Insert }

updateVILine :: ByteString -> VILine -> VILine
updateVILine newcontent VILine {..} = VILine
  { _viMode
  , _viCursorPos   = _viCursorPos + (BS.length newcontent - BS.length _viLineContent)
  , _viLineContent = newcontent
  }
