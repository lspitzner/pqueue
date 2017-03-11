{-# LANGUAGE CPP #-}
module Data.PQueue.Top where

import Control.DeepSeq (NFData(rnf))
import qualified Data.Ord as Ord
import Prelude hiding (compare)

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data, Typeable)
#endif

class Top top where
  compare :: (Ord a) => Wrap top a -> Wrap top a -> Ordering

newtype Wrap top a = Wrap {unwrap :: a}
  deriving (Data, Typeable)

data Min = Min
data Max = Max

instance Top Min where compare (Wrap x) (Wrap y) = Ord.compare x y
instance Top Max where compare (Wrap x) (Wrap y) = Ord.compare y x

instance (Top top, Eq a) => Eq (Wrap top a) where
  Wrap x == Wrap y  =  x==y

instance (Top top, Ord a) => Ord (Wrap top a) where
  compare = compare

instance NFData a => NFData (Wrap top a) where
  rnf (Wrap a) = rnf a

instance Functor (Wrap top) where
  fmap f (Wrap a) = Wrap (f a)
