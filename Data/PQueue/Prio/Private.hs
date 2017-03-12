{-# LANGUAGE CPP #-}
module Data.PQueue.Prio.Private (
  PQueue(PQ), toList, fromOrderedList, empty, union, unions,
  ) where

import qualified Data.PQueue.Prio.Min as Q
import Data.PQueue.Top (Top, Wrap(Wrap, unwrap))

import Control.DeepSeq (NFData (rnf))
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.Traversable (Traversable(traverse))
import Data.Foldable (Foldable, foldr, foldl)

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null, foldr, foldl)


#ifdef __GLASGOW_HASKELL__
import Data.Data (Data, Typeable)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)


-- | A priority queue where values of type @a@ are annotated with keys of type @k@.
-- The queue supports extracting the element with top key.
newtype PQueue top k a = PQ (Q.MinPQueue (Wrap top k) a)
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data, Typeable)
# else
  deriving (Eq, Ord)
# endif

instance (NFData k, NFData a) => NFData (PQueue top k a) where
  rnf (PQ q) = rnf q


instance (Top top, Ord k) => Monoid (PQueue top k a) where
  mempty = empty
  mappend = union
  mconcat = unions

instance (Top top, Ord k, Show k, Show a) => Show (PQueue top k a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromOrderedList " . shows (toList xs)

instance (Read k, Read a) => Read (PQueue top k a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromOrderedList" <- lexP
    xs <- readPrec
    return (fromOrderedList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromOrderedList",s) <- lex r
    (xs,t) <- reads s
    return (fromOrderedList xs,t)
#endif

instance Functor (PQueue top k) where
  fmap f (PQ q) = PQ (fmap f q)

instance (Top top, Ord k) => Foldable (PQueue top k) where
  foldr f z (PQ q) = foldr f z q
  foldl f z (PQ q) = foldl f z q

instance (Top top, Ord k) => Traversable (PQueue top k) where
  traverse f (PQ q) = PQ <$> traverse f q

-- | /O(1)/.  Returns the empty priority queue.
empty :: PQueue top k a
empty = PQ Q.empty

-- | Amortized /O(log(min(n1, n2)))/, worst-case /O(log(max(n1, n2)))/.  Returns the union
-- of the two specified queues.
union :: (Top top, Ord k) => PQueue top k a -> PQueue top k a -> PQueue top k a
PQ q1 `union` PQ q2 = PQ (q1 `Q.union` q2)

-- | The union of a list of queues: (@'unions' == 'List.foldl' 'union' 'empty'@).
unions :: (Top top, Ord k) => [PQueue top k a] -> PQueue top k a
unions qs = PQ (Q.unions [q | PQ q <- qs])

-- | /O(n)/.  Build a priority queue from a list of (key, value) pairs where every suffix contains the top element at the list head.  /The precondition is not checked./
fromOrderedList :: [(k, a)] -> PQueue top k a
fromOrderedList = PQ . Q.fromAscList . fmap (first' Wrap)

-- | /O(n log n)/.  Return all (key, value) pairs in natural order by key.
--
-- If the traversal order is irrelevant, consider using 'toListU'.
toList :: (Top top, Ord k) => PQueue top k a -> [(k, a)]
toList (PQ q) = fmap (first' unwrap) (Q.toAscList q)
