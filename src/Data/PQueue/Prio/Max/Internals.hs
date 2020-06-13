{-# LANGUAGE CPP #-}

module Data.PQueue.Prio.Max.Internals where

import Control.DeepSeq (NFData(rnf))

import Data.Traversable (Traversable(traverse))
import Data.Foldable (Foldable(foldr, foldl))
import Data.Functor ((<$>))
# if __GLASGOW_HASKELL__
import Data.Data (Data, Typeable)
# endif

import Prelude hiding (foldr, foldl)

import Data.PQueue.Prio.Internals (MinPQueue)

newtype Down a = Down { unDown :: a }
# if __GLASGOW_HASKELL__
  deriving (Eq, Data, Typeable)
# else
  deriving (Eq)
# endif

-- | A priority queue where values of type @a@ are annotated with keys of type @k@.
-- The queue supports extracting the element with maximum key.
newtype MaxPQueue k a = MaxPQ (MinPQueue (Down k) a)
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data, Typeable)
# else
  deriving (Eq, Ord)
# endif

instance (NFData k, NFData a) => NFData (MaxPQueue k a) where
  rnf (MaxPQ q) = rnf q

instance NFData a => NFData (Down a) where
  rnf (Down a) = rnf a

instance Ord a => Ord (Down a) where
  Down a `compare` Down b = b `compare` a
  Down a <= Down b = b <= a

instance Functor Down where
  fmap f (Down a) = Down (f a)

instance Foldable Down where
  foldr f z (Down a) = a `f` z
  foldl f z (Down a) = z `f` a

instance Traversable Down where
  traverse f (Down a) = Down <$> f a
