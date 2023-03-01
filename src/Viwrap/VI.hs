module Viwrap.VI
  ( AutoComplete (..)
  , VIEdit (..)
  , VIHook (..)
  , VILine (..)
  , VIMode (..)
  , Zipper (..)
  , backspace
  , backwardZipper
  , deleteZipper
  , forwardZipper
  , initialVILine
  , insertBS
  , insertZipper
  , moveLeft
  , moveRight
    -- , updateVILine
  , viLineContent
  , viMode
  , zipperCrumbs
  , zipperFocus
  ) where

import Control.Monad.Freer.TH (makeEffect)
import Data.ByteString        (ByteString)
import Data.ByteString        qualified as BS
import Lens.Micro             ((%~))
import Lens.Micro.TH          (makeLenses)

data VIMode
  = Normal
  | Insert
  | Visual
  deriving stock (Eq, Ord, Show)

data Zipper
  = Zipper
      { _zipperFocus  :: ByteString
      , _zipperCrumbs :: ByteString
      }
  deriving stock (Eq, Show)

makeLenses ''Zipper

instance Semigroup Zipper where
  (Zipper f1 c1) <> (Zipper f2 c2) = Zipper (f1 <> f2) (c1 <> c2)

instance Monoid Zipper where
  mempty = Zipper mempty mempty

forwardZipper :: Int -> Zipper -> Zipper
forwardZipper n Zipper {..} =
  let crumb    = BS.take n _zipperFocus
      newfocus = BS.drop n _zipperFocus
  in  Zipper { _zipperFocus = newfocus, _zipperCrumbs = BS.reverse crumb <> _zipperCrumbs }

backwardZipper :: Int -> Zipper -> Zipper
backwardZipper n Zipper {..} =
  let crumb     = BS.reverse $ BS.take n _zipperCrumbs
      newcrumbs = BS.drop n _zipperCrumbs
  in  Zipper { _zipperFocus = crumb <> _zipperFocus, _zipperCrumbs = newcrumbs }


insertZipper :: ByteString -> Zipper -> Zipper
insertZipper content Zipper {..} = Zipper _zipperFocus (BS.reverse content <> _zipperCrumbs)

deleteZipper :: Zipper -> Zipper
deleteZipper = zipperCrumbs %~ BS.drop 1


data VILine
  = VILine
      { _viLineContent :: Zipper
      , _viMode        :: VIMode
      }
  deriving stock (Eq, Show)

makeLenses ''VILine

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

initialVILine :: VILine
initialVILine = VILine { _viLineContent = mempty, _viMode = Insert }
