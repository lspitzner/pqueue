-- | Writing 'Foldable' instances for non-regular (AKA, nested) types in the
-- natural manner leads to full `Foldable` dictionaries being constructed on
-- each recursive call. This is pretty inefficient. It's better to construct
-- exactly what we need instead.
module Data.PQueue.Internals.Foldable
  ( Foldr (..)
  , Foldl (..)
  , FoldMap (..)
  , Foldl' (..)
  ) where

class Foldr t where
  foldr_ :: (a -> b -> b) -> b -> t a -> b

class Foldl t where
  foldl_ :: (b -> a -> b) -> b -> t a -> b

class FoldMap t where
  foldMap_ :: Monoid m => (a -> m) -> t a -> m

class Foldl' t where
  foldl'_ :: (b -> a -> b) -> b -> t a -> b
