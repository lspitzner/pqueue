{-# LANGUAGE CPP #-}
module Data.PQueue.Top where

import Control.DeepSeq (NFData(rnf))

import qualified Data.Foldable as Fold

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data, Typeable)
#endif

class Top top where
  switch :: f Min -> f Max -> f top

newtype Wrap top a = Wrap {unwrap :: a}
  deriving (Data, Typeable)

data Min = Min
data Max = Max

instance Top Min where switch f _ = f
instance Top Max where switch _ f = f


instance (Top top, Eq a) => Eq (Wrap top a) where
  Wrap x == Wrap y  =  x==y

newtype
  Compare a top = Compare {runCompare :: Wrap top a -> Wrap top a -> Ordering}

instance (Top top, Ord a) => Ord (Wrap top a) where
  compare =
    runCompare $
    switch
      (Compare $ \(Wrap x) (Wrap y) -> compare x y)
      (Compare $ \(Wrap x) (Wrap y) -> compare y x)

instance NFData a => NFData (Wrap top a) where
  rnf (Wrap a) = rnf a

instance Functor (Wrap top) where
  fmap f (Wrap a) = Wrap (f a)

instance Fold.Foldable (Wrap top) where
  foldMap f (Wrap a) = f a
  foldr f z (Wrap a) = a `f` z
  foldl f z (Wrap a) = z `f` a
