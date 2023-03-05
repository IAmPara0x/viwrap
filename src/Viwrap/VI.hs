module Viwrap.VI
  ( AutoComplete (..)
  , VIEdit (..)
  , VIHook (..)
  , VILine
  , VILines
  , VIMode (..)
  , VIState (..)
  , backspace
  , currentLine
  , initialVIState
  , insertBS
  , moveLeft
  , moveRight
  , prevLines
  , viMode
  ) where

import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString        (ByteString)
import Data.Sequence          (Seq)
import Lens.Micro.TH          (makeLenses)

import Viwrap.Zipper

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Eq, Ord, Show)


type VILine = Zipper ByteString


data VIEdit a where
  Backspace :: VIEdit ()
  MoveLeft :: Int -> VIEdit ()
  MoveRight :: Int -> VIEdit ()
  InsertBS :: ByteString -> VIEdit ()

makeEffect ''VIEdit

data AutoComplete
  = Completion ByteString
  | CompletionList
  deriving stock (Eq, Show)

data VIHook
  = SyncCursor
  | TabPressed
  deriving stock (Eq, Show)

type VILines = Zipper (Seq VILine)

data VIState
  = VIState
      { _viMode      :: VIMode
      , _prevLines   :: VILines
      , _currentLine :: VILine
      }
  deriving stock (Eq, Show)

initialVIState :: VIState
initialVIState = VIState { _viMode = Insert, _prevLines = mempty, _currentLine = mempty }

makeLenses ''VIState
