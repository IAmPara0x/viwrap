module Viwrap.Zipper
  ( IsZipper (..)
  , Zipper (..)
  , appendZipper
  , backwardZipper
  , contentZipper
  , deleteZipper
  , forwardZipper
  , insertZipper
  , zipperCrumbs
  , zipperFocus
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Sequence   (Seq)
import Data.Sequence   qualified as Seq
import Lens.Micro      ((%~))
import Lens.Micro.TH   (makeLenses)

class (Monoid a) => IsZipper a where

  zipperTake :: Int -> a -> a
  zipperDrop :: Int -> a -> a
  zipperReverse :: a -> a

instance IsZipper ByteString where
  zipperTake    = BS.take
  zipperDrop    = BS.drop
  zipperReverse = BS.reverse

instance IsZipper (Seq a) where

  zipperTake    = Seq.take
  zipperDrop    = Seq.drop
  zipperReverse = Seq.reverse

data Zipper a
  = Zipper
      { _zipperFocus  :: a
      , _zipperCrumbs :: a
      }
  deriving stock (Eq, Show)

makeLenses ''Zipper

instance Semigroup a => Semigroup (Zipper a) where
  (Zipper f1 c1) <> (Zipper f2 c2) = Zipper (f1 <> f2) (c1 <> c2)

instance Monoid a => Monoid (Zipper a) where
  mempty = Zipper mempty mempty

forwardZipper :: (IsZipper a) => Int -> Zipper a -> Zipper a
forwardZipper n Zipper {..} =
  let crumb    = zipperTake n _zipperFocus
      newfocus = zipperDrop n _zipperFocus
  in  Zipper { _zipperFocus = newfocus, _zipperCrumbs = zipperReverse crumb <> _zipperCrumbs }

backwardZipper :: (IsZipper a) => Int -> Zipper a -> Zipper a
backwardZipper n Zipper {..} =
  let crumb     = zipperReverse $ zipperTake n _zipperCrumbs
      newcrumbs = zipperDrop n _zipperCrumbs
  in  Zipper { _zipperFocus = crumb <> _zipperFocus, _zipperCrumbs = newcrumbs }

insertZipper :: IsZipper a => a -> Zipper a -> Zipper a
insertZipper content Zipper {..} = Zipper _zipperFocus (zipperReverse content <> _zipperCrumbs)

appendZipper :: IsZipper a => a -> Zipper a -> Zipper a
appendZipper content Zipper {..} = Zipper (content <> _zipperFocus) _zipperCrumbs

deleteZipper :: IsZipper a => Zipper a -> Zipper a
deleteZipper = zipperCrumbs %~ zipperDrop 1

contentZipper :: IsZipper a => Zipper a -> a
contentZipper Zipper {..} = zipperReverse _zipperCrumbs <> _zipperFocus
