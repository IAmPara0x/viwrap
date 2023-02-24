module Viwrap.VI
  ( VIEdit (..)
  , VILine (..)
  , VIMode (..)
  , backspace
  , initialVILine
  , insertBS
  , moveLeft
  , moveRight
  , viCursorPos
  , viLineContents
  , viMode
  ) where

import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString        (ByteString)
import Lens.Micro.TH          (makeLenses)

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Eq, Ord, Show)

data VILine
  = VILine
      { _viLineContents :: ByteString
      , _viCursorPos    :: Int
      , _viMode         :: VIMode
      }
  deriving stock (Show)

makeLenses ''VILine

data VIEdit a where
  Backspace :: VIEdit VILine
  MoveLeft :: VIEdit VILine
  MoveRight :: VIEdit VILine
  InsertBS :: ByteString -> VIEdit VILine

makeEffect ''VIEdit


initialVILine :: VILine
initialVILine = VILine { _viLineContents = mempty, _viCursorPos = 0, _viMode = Insert }
