{-# LANGUAGE CPP #-}
module Data.PQueue.Top where

import Control.DeepSeq (NFData(rnf))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data, Typeable)
#endif

-- class First first where
--   switch :: f Min -> f Max -> f first

class First first where
  switch :: a -> a -> TaggedF first a

newtype TaggedF first a = TaggedF {unTaggedF :: a}
  deriving (Data, Typeable)

data Min = Min
data Max = Max

-- instance First Min where switch f _ = f
-- instance First Max where switch _ f = f

instance First Min where switch f _ = TaggedF f
instance First Max where switch _ f = TaggedF f

instance (First first, Eq a) => Eq (TaggedF first a) where
  TaggedF x == TaggedF y  =  x==y

-- newtype
--   Compare a first = Compare {runCompare :: TaggedF first a -> TaggedF first a -> Ordering}

instance (First first, Ord a) => Ord (TaggedF first a) where
  compare x y = unTaggedF $ switch compare (flip compare) <*> x <*> y
    -- runCompare $
    -- switch
    --   (Compare $ \(TaggedF x) (TaggedF y) -> compare x y)
    --   (Compare $ \(TaggedF x) (TaggedF y) -> compare y x)

instance NFData a => NFData (TaggedF first a) where
  rnf (TaggedF a) = rnf a

instance Functor (TaggedF first) where
  fmap f (TaggedF a) = TaggedF (f a)

instance Applicative (TaggedF s) where
  pure = TaggedF
  TaggedF f <*> TaggedF x = TaggedF (f x)
  _ *> n = n

instance Fold.Foldable (TaggedF first) where
  foldMap f (TaggedF a) = f a
  foldr f z (TaggedF a) = a `f` z
  foldl f z (TaggedF a) = z `f` a

instance Trav.Traversable (TaggedF first) where
  traverse f (TaggedF a) = fmap TaggedF $ f a
