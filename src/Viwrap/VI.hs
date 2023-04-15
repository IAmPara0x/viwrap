module Viwrap.VI
  ( AutoComplete (..)
  , VIHook (..)
  , VILine
  , VILines
  , VIMode (..)
  , VIState (..)
  , currentLine
  , prevLines
  , viMode
  ) where

import Data.ByteString (ByteString)
import Data.Default    (Default (def))
import Data.Sequence   (Seq)
import Lens.Micro.TH   (makeLenses)

import Viwrap.Zipper

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Eq, Ord, Show)


type VILine = Zipper ByteString

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

instance Default VIState where
  def = VIState { _viMode = Insert, _prevLines = mempty, _currentLine = mempty }

makeLenses ''VIState
