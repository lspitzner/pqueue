-- | Writing `Foldable`/`Functor` instances for non-regular (AKA, nested) types in the
-- natural manner leads to full dictionaries being constructed on
-- each recursive call. This is pretty inefficient. It's better to construct
-- exactly what we need instead.
module Data.PQueue.Internals.Classes
  ( Foldr(..)
  , Foldl(..)
  , FoldMap(..)
  , Foldl'(..)
  , Fmap(..)
  ) where

class Foldr t where
  foldr_ :: (a -> b -> b) -> b -> t a -> b

class Foldl t where
  foldl_ :: (b -> a -> b) -> b -> t a -> b

class FoldMap t where
  foldMap_ :: Monoid m => (a -> m) -> t a -> m

class Foldl' t where
  foldl'_ :: (b -> a -> b) -> b -> t a -> b

class Fmap f where
  fmap_ :: (a -> b) -> f a -> f b
