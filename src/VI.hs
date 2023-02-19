module VI
  (
  ) where

import Data.ByteString (ByteString)

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Show)

data VILine
  = VILine
      { _viLineContents :: ByteString
      , _viCursorPos    :: Int
      , _viMode         :: VIMode
      }
  deriving stock (Show)

-- data wwwwww

data VIEdit a where
  Backspace :: VILine -> VIEdit VILine
  MoveLeft :: VILine -> VIEdit VILine
  MoveRight :: VILine -> VIEdit VILine
