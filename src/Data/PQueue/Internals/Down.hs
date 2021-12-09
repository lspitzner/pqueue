{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.PQueue.Internals.Down where

import Control.DeepSeq (NFData(rnf))
import Data.Foldable (Foldable (..))

#if __GLASGOW_HASKELL__
import Data.Data (Data)
#endif

newtype Down a = Down { unDown :: a }
# if __GLASGOW_HASKELL__
  deriving (Eq, Data)
# else
  deriving (Eq)
# endif


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
  foldl' f !z (Down a) = z `f` a
