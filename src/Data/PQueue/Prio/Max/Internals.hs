{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Prio.Max
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.PQueue.Prio.Max.Internals (
  MaxPQueue (..),
  -- * Construction
  empty,
  singleton,
  insert,
  insertBehind,
  union,
  unions,
  -- * Query
  null,
  size,
  -- ** Maximum view
  findMax,
  getMax,
  deleteMax,
  deleteFindMax,
  adjustMax,
  adjustMaxA,
  adjustMaxWithKey,
  adjustMaxWithKeyA,
  updateMax,
  updateMaxA,
  updateMaxWithKey,
  updateMaxWithKeyA,
  maxView,
  maxViewWithKey,
  -- * Traversal
  -- ** Map
  map,
  mapWithKey,
  mapKeys,
  mapKeysMonotonic,
  -- ** Fold
  foldrWithKey,
  foldlWithKey,
  -- ** Traverse
  traverseWithKey,
  mapMWithKey,
  -- * Subsets
  -- ** Indexed
  take,
  drop,
  splitAt,
  -- ** Predicates
  takeWhile,
  takeWhileWithKey,
  dropWhile,
  dropWhileWithKey,
  span,
  spanWithKey,
  break,
  breakWithKey,
  -- *** Filter
  filter,
  filterWithKey,
  partition,
  partitionWithKey,
  mapMaybe,
  mapMaybeWithKey,
  mapEither,
  mapEitherWithKey,
  -- * List operations
  -- ** Conversion from lists
  fromList,
  fromAscList,
  fromDescList,
  -- ** Conversion to lists
  keys,
  elems,
  assocs,
  toAscList,
  toDescList,
  toList,
  -- * Unordered operations
  foldrU,
  foldMapWithKeyU,
  foldrWithKeyU,
  foldlU,
  foldlU',
  foldlWithKeyU,
  foldlWithKeyU',
  traverseU,
  traverseWithKeyU,
  keysU,
  elemsU,
  assocsU,
  toListU,
  -- * Helper methods
  seqSpine
  )
  where

import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.PQueue.Internals.Down
import Data.PQueue.Prio.Internals (MinPQueue)
import qualified Data.PQueue.Prio.Internals as PrioInternals
import Control.DeepSeq (NFData(rnf))

import Data.Semigroup (Semigroup(..), stimesMonoid)

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null)
import qualified Data.Foldable as F

import qualified Data.PQueue.Prio.Min as Q

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
#endif

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

#ifndef __GLASGOW_HASKELL__
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

-- | A priority queue where values of type @a@ are annotated with keys of type @k@.
-- The queue supports extracting the element with maximum key.
newtype MaxPQueue k a = MaxPQ (MinPQueue (Down k) a)
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data)
# else
  deriving (Eq, Ord)
# endif

instance (NFData k, NFData a) => NFData (MaxPQueue k a) where
  rnf (MaxPQ q) = rnf q

instance Ord k => Semigroup (MaxPQueue k a) where
  (<>) = union
  stimes = stimesMonoid
  {-# INLINABLE stimes #-}

instance Ord k => Monoid (MaxPQueue k a) where
  mempty = empty
#if !MIN_VERSION_base(4,11,0)
  mappend = union
#endif
  mconcat = unions

instance (Ord k, Show k, Show a) => Show (MaxPQueue k a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromDescList " . shows (toDescList xs)

instance (Read k, Read a) => Read (MaxPQueue k a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromDescList" <- lexP
    xs <- readPrec
    return (fromDescList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromDescList",s) <- lex r
    (xs,t) <- reads s
    return (fromDescList xs,t)
#endif

instance Functor (MaxPQueue k) where
  fmap f (MaxPQ q) = MaxPQ (fmap f q)

instance FunctorWithIndex k (MaxPQueue k) where
  imap = mapWithKey

instance Ord k => Foldable (MaxPQueue k) where
  foldr f z (MaxPQ q) = foldr f z q
  foldl f z (MaxPQ q) = foldl f z q
  length = size
  null = null

instance Ord k => FoldableWithIndex k (MaxPQueue k) where
  ifoldr   = foldrWithKey
  ifoldl f = foldlWithKey (flip f)

-- | Traverses in descending order. 'mapM' is strictly accumulating like
-- 'mapMWithKey'.
instance Ord k => Traversable (MaxPQueue k) where
  traverse f (MaxPQ q) = MaxPQ <$> traverse f q
  mapM = mapMWithKey . const
  sequence = mapM id

instance Ord k => TraversableWithIndex k (MaxPQueue k) where
  itraverse = traverseWithKey

-- | \(O(1)\). Returns the empty priority queue.
empty :: MaxPQueue k a
empty = MaxPQ Q.empty

-- | \(O(1)\). Constructs a singleton priority queue.
singleton :: k -> a -> MaxPQueue k a
singleton = coerce Q.singleton

-- | Amortized \(O(1)\), worst-case \(O(\log n)\). Inserts
-- an element with the specified key into the queue.
insert :: Ord k => k -> a -> MaxPQueue k a -> MaxPQueue k a
insert = coerce Q.insert

-- | \(O(n)\) (an earlier implementation had \(O(1)\) but was buggy).
-- Insert an element with the specified key into the priority queue,
-- putting it behind elements whose key compares equal to the
-- inserted one.
{-# DEPRECATED insertBehind "This function is not reliable." #-}
insertBehind :: Ord k => k -> a -> MaxPQueue k a -> MaxPQueue k a
insertBehind = coerce Q.insertBehind

-- | Amortized \(O(\log \min(n_1,n_2))\), worst-case \(O(\log \max(n_1,n_2))\). Returns the union
-- of the two specified queues.
union :: Ord k => MaxPQueue k a -> MaxPQueue k a -> MaxPQueue k a
union = coerce Q.union

-- | The union of a list of queues: (@'unions' == 'List.foldl' 'union' 'empty'@).
unions :: Ord k => [MaxPQueue k a] -> MaxPQueue k a
unions = coerce Q.unions

-- | \(O(1)\). Checks if this priority queue is empty.
null :: MaxPQueue k a -> Bool
null (MaxPQ q) = Q.null q

-- | \(O(1)\). Returns the size of this priority queue.
size :: MaxPQueue k a -> Int
size (MaxPQ q) = Q.size q

-- | \(O(1)\). The maximal (key, element) in the queue. Calls 'error' if empty.
findMax :: MaxPQueue k a -> (k, a)
findMax = fromMaybe (error "Error: findMax called on an empty queue") . getMax

-- | \(O(1)\). The maximal (key, element) in the queue, if the queue is nonempty.
getMax :: MaxPQueue k a -> Maybe (k, a)
getMax = coerce Q.getMin

-- | \(O(\log n)\). Delete and find the element with the maximum key. Calls 'error' if empty.
deleteMax :: Ord k => MaxPQueue k a -> MaxPQueue k a
deleteMax = coerce Q.deleteMin

-- | \(O(\log n)\). Delete and find the element with the maximum key. Calls 'error' if empty.
deleteFindMax :: Ord k => MaxPQueue k a -> ((k, a), MaxPQueue k a)
deleteFindMax = fromMaybe (error "Error: deleteFindMax called on an empty queue") . maxViewWithKey

-- | \(O(1)\). Alter the value at the maximum key. If the queue is empty, does nothing.
adjustMax :: (a -> a) -> MaxPQueue k a -> MaxPQueue k a
adjustMax = adjustMaxWithKey . const

-- | \(O(1)\) per operation. Alter the value at the maximum key in an
-- 'Applicative' context. If the queue is empty, does nothing.
--
-- @since 1.4.2
adjustMaxA :: Applicative f => (a -> f a) -> MaxPQueue k a -> f (MaxPQueue k a)
adjustMaxA = adjustMaxWithKeyA . const

-- | \(O(1)\). Alter the value at the maximum key. If the queue is empty, does nothing.
adjustMaxWithKey :: (k -> a -> a) -> MaxPQueue k a -> MaxPQueue k a
adjustMaxWithKey = coerce Q.adjustMinWithKey

-- | \(O(1)\) per operation. Alter the value at the maximum key in an
-- 'Applicative' context. If the queue is empty, does nothing.
--
-- @since 1.4.2
adjustMaxWithKeyA :: Applicative f => (k -> a -> f a) -> MaxPQueue k a -> f (MaxPQueue k a)
adjustMaxWithKeyA f (MaxPQ q) = PrioInternals.adjustMinWithKeyA' MaxPQ (coerce f) q

-- | \(O(\log n)\). (Actually \(O(1)\) if there's no deletion.) Update the value at the maximum key.
-- If the queue is empty, does nothing.
updateMax :: Ord k => (a -> Maybe a) -> MaxPQueue k a -> MaxPQueue k a
updateMax = updateMaxWithKey . const

-- | \(O(\log n)\) per operation. (Actually \(O(1)\) if there's no deletion.) Update
-- the value at the maximum key in an 'Applicative' context. If the queue is
-- empty, does nothing.
--
-- @since 1.4.2
updateMaxA :: (Applicative f, Ord k) => (a -> f (Maybe a)) -> MaxPQueue k a -> f (MaxPQueue k a)
updateMaxA = updateMaxWithKeyA . const

-- | \(O(\log n)\). (Actually \(O(1)\) if there's no deletion.) Update the value at the maximum key.
-- If the queue is empty, does nothing.
updateMaxWithKey :: Ord k => (k -> a -> Maybe a) -> MaxPQueue k a -> MaxPQueue k a
updateMaxWithKey = coerce Q.updateMinWithKey

-- | \(O(\log n)\) per operation. (Actually \(O(1)\) if there's no deletion.) Update
-- the value at the maximum key in an 'Applicative' context. If the queue is
-- empty, does nothing.
--
-- @since 1.4.2
updateMaxWithKeyA :: (Applicative f, Ord k) => (k -> a -> f (Maybe a)) -> MaxPQueue k a -> f (MaxPQueue k a)
updateMaxWithKeyA f (MaxPQ q) = PrioInternals.updateMinWithKeyA' MaxPQ (coerce f) q

-- | \(O(\log n)\). Retrieves the value associated with the maximum key of the queue, and the queue
-- stripped of that element, or 'Nothing' if passed an empty queue.
maxView :: Ord k => MaxPQueue k a -> Maybe (a, MaxPQueue k a)
maxView q = do
  ((_, a), q') <- maxViewWithKey q
  return (a, q')

-- | \(O(\log n)\). Retrieves the maximal (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
maxViewWithKey :: Ord k => MaxPQueue k a -> Maybe ((k, a), MaxPQueue k a)
maxViewWithKey = coerce Q.minViewWithKey

-- | \(O(n)\). Map a function over all values in the queue.
map :: (a -> b) -> MaxPQueue k a -> MaxPQueue k b
map = mapWithKey . const

-- | \(O(n)\). Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> MaxPQueue k a -> MaxPQueue k b
mapWithKey = coerce Q.mapWithKey

-- | \(O(n)\). Map a function over all values in the queue.
mapKeys :: Ord k' => (k -> k') -> MaxPQueue k a -> MaxPQueue k' a
mapKeys = coerce Q.mapKeys

-- | \(O(n)\). @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when
-- @f@ is (weakly) monotonic (meaning that @x <= y@ implies @f x <= f y@).
-- /The precondition is not checked./ This function has better performance than 'mapKeys'.
--
-- Note: if the given function returns bottom for any of the keys in the queue, then the
-- portion of the queue which is bottom is /unspecified/.
mapKeysMonotonic :: (k -> k') -> MaxPQueue k a -> MaxPQueue k' a
mapKeysMonotonic = coerce Q.mapKeysMonotonic

-- | \(O(n \log n)\). Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toDescList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> MaxPQueue k a -> b
foldrWithKey f z (MaxPQ q) = Q.foldrWithKey (coerce f) z q

-- | \(O(n \log n)\). Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toDescList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: Ord k => (b -> k -> a -> b) -> b -> MaxPQueue k a -> b
foldlWithKey f z0 (MaxPQ q) = Q.foldlWithKey (coerce f) z0 q

-- | \(O(n \log n)\). Traverses the elements of the queue in descending order by key.
-- (@'traverseWithKey' f q == 'fromDescList' <$> 'traverse' ('uncurry' f) ('toDescList' q)@)
--
-- If you do not care about the /order/ of the traversal, consider using 'traverseWithKeyU'.
--
-- If you are working in a strict monad, consider using 'mapMWithKey'.
traverseWithKey :: (Ord k, Applicative f) => (k -> a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseWithKey f (MaxPQ q) = MaxPQ <$> Q.traverseWithKey (coerce f) q

-- | A strictly accumulating version of 'traverseWithKey'. This works well in
-- 'IO' and strict @State@, and is likely what you want for other "strict" monads,
-- where @⊥ >>= pure () = ⊥@.
mapMWithKey :: (Ord k, Monad m) => (k -> a -> m b) -> MaxPQueue k a -> m (MaxPQueue k b)
mapMWithKey f (MaxPQ q) = MaxPQ <$> Q.mapMWithKey (coerce f) q

-- | \(O(k \log n)\). Takes the first @k@ (key, value) pairs in the queue, or the first @n@ if @k >= n@.
-- (@'take' k q == 'List.take' k ('toDescList' q)@)
take :: Ord k => Int -> MaxPQueue k a -> [(k, a)]
take = coerce Q.take

-- | \(O(k \log n)\). Deletes the first @k@ (key, value) pairs in the queue, or returns an empty queue if @k >= n@.
drop :: Ord k => Int -> MaxPQueue k a -> MaxPQueue k a
drop = coerce Q.drop

-- | \(O(k \log n)\). Equivalent to @('take' k q, 'drop' k q)@.
splitAt :: Ord k => Int -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
splitAt = coerce Q.splitAt

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (p . 'snd') ('toDescList' q)@)
takeWhile :: Ord k => (a -> Bool) -> MaxPQueue k a -> [(k, a)]
takeWhile = takeWhileWithKey . const

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (uncurry p) ('toDescList' q)@)
takeWhileWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> [(k, a)]
takeWhileWithKey = coerce Q.takeWhileWithKey

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhile :: Ord k => (a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
dropWhile = dropWhileWithKey . const

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhileWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
dropWhileWithKey = coerce Q.dropWhileWithKey

-- | Equivalent to @('takeWhile' p q, 'dropWhile' p q)@.
span :: Ord k => (a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
span = spanWithKey . const

-- | Equivalent to @'span' ('not' . p)@.
break :: Ord k => (a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
break = breakWithKey . const

-- | Equivalent to @'spanWithKey' (\k a -> 'not' (p k a)) q@.
spanWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
spanWithKey = coerce Q.spanWithKey

-- | Equivalent to @'spanWithKey' (\k a -> 'not' (p k a)) q@.
breakWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
breakWithKey = coerce Q.breakWithKey

-- | \(O(n)\). Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
filter = filterWithKey . const

-- | \(O(n)\). Filter all values that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
filterWithKey = coerce Q.filterWithKey

-- | \(O(n)\). Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partition :: Ord k => (a -> Bool) -> MaxPQueue k a -> (MaxPQueue k a, MaxPQueue k a)
partition = partitionWithKey . const

-- | \(O(n)\). Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> (MaxPQueue k a, MaxPQueue k a)
partitionWithKey = coerce Q.partitionWithKey

-- | \(O(n)\). Map values and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> MaxPQueue k a -> MaxPQueue k b
mapMaybe = mapMaybeWithKey . const

-- | \(O(n)\). Map values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> MaxPQueue k a -> MaxPQueue k b
mapMaybeWithKey = coerce Q.mapMaybeWithKey

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> MaxPQueue k a -> (MaxPQueue k b, MaxPQueue k c)
mapEither = mapEitherWithKey . const

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> MaxPQueue k a -> (MaxPQueue k b, MaxPQueue k c)
mapEitherWithKey = coerce Q.mapEitherWithKey

-- | \(O(n)\). Build a priority queue from the list of (key, value) pairs.
fromList :: Ord k => [(k, a)] -> MaxPQueue k a
fromList = coerce Q.fromList

-- | \(O(n)\). Build a priority queue from an ascending list of (key, value) pairs. /The precondition is not checked./
fromAscList :: [(k, a)] -> MaxPQueue k a
fromAscList = coerce Q.fromDescList

-- | \(O(n)\). Build a priority queue from a descending list of (key, value) pairs. /The precondition is not checked./
fromDescList :: [(k, a)] -> MaxPQueue k a
fromDescList = coerce Q.fromAscList

-- | \(O(n \log n)\). Return all keys of the queue in descending order.
keys :: Ord k => MaxPQueue k a -> [k]
keys = fmap fst . toDescList

-- | \(O(n \log n)\). Return all elements of the queue in descending order by key.
elems :: Ord k => MaxPQueue k a -> [a]
elems = fmap snd . toDescList

-- | \(O(n \log n)\). Equivalent to 'toDescList'.
assocs :: Ord k => MaxPQueue k a -> [(k, a)]
assocs = toDescList

-- | \(O(n \log n)\). Return all (key, value) pairs in ascending order by key.
toAscList :: Ord k => MaxPQueue k a -> [(k, a)]
toAscList = coerce Q.toDescList

-- | \(O(n \log n)\). Return all (key, value) pairs in descending order by key.
toDescList :: Ord k => MaxPQueue k a -> [(k, a)]
toDescList = coerce Q.toAscList

-- | \(O(n \log n)\). Equivalent to 'toDescList'.
--
-- If the traversal order is irrelevant, consider using 'toListU'.
toList :: Ord k => MaxPQueue k a -> [(k, a)]
toList = toDescList

-- | \(O(n)\). An unordered right fold over the elements of the queue, in no particular order.
foldrU :: (a -> b -> b) -> b -> MaxPQueue k a -> b
foldrU = foldrWithKeyU . const

-- | \(O(n)\). An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> MaxPQueue k a -> b
foldrWithKeyU f z (MaxPQ q) = Q.foldrWithKeyU (coerce f) z q

-- | \(O(n)\). An unordered monoidal fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldMapWithKeyU :: Monoid m => (k -> a -> m) -> MaxPQueue k a -> m
foldMapWithKeyU f (MaxPQ q) = Q.foldMapWithKeyU (coerce f) q

-- | \(O(n)\). An unordered left fold over the elements of the queue, in no
-- particular order. This is rarely what you want; 'foldrU' and 'foldlU'' are
-- more likely to perform well.
foldlU :: (b -> a -> b) -> b -> MaxPQueue k a -> b
foldlU f = foldlWithKeyU (const . f)

-- | \(O(n)\). An unordered strict left fold over the elements of the queue, in no
-- particular order.
--
-- @since 1.4.2
foldlU' :: (b -> a -> b) -> b -> MaxPQueue k a -> b
foldlU' f = foldlWithKeyU' (const . f)

-- | \(O(n)\). An unordered left fold over the elements of the queue, in no
-- particular order. This is rarely what you want; 'foldrWithKeyU' and
-- 'foldlWithKeyU'' are more likely to perform well.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> MaxPQueue k a -> b
foldlWithKeyU f z0 (MaxPQ q) = Q.foldlWithKeyU (coerce f) z0 q

-- | \(O(n)\). An unordered left fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldlWithKeyU' :: (b -> k -> a -> b) -> b -> MaxPQueue k a -> b
foldlWithKeyU' f z0 (MaxPQ q) = Q.foldlWithKeyU' (coerce f) z0 q

-- | \(O(n)\). An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseU :: (Applicative f) => (a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseU = traverseWithKeyU . const

-- | \(O(n)\). An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseWithKeyU :: (Applicative f) => (k -> a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseWithKeyU f (MaxPQ q) = MaxPQ <$> Q.traverseWithKeyU (coerce f) q

-- | \(O(n)\). Return all keys of the queue in no particular order.
keysU :: MaxPQueue k a -> [k]
keysU = fmap fst . toListU

-- | \(O(n)\). Return all elements of the queue in no particular order.
elemsU :: MaxPQueue k a -> [a]
elemsU = fmap snd . toListU

-- | \(O(n)\). Equivalent to 'toListU'.
assocsU :: MaxPQueue k a -> [(k, a)]
assocsU = toListU

-- | \(O(n)\). Returns all (key, value) pairs in the queue in no particular order.
toListU :: MaxPQueue k a -> [(k, a)]
toListU = coerce Q.toListU

-- | \(O(\log n)\). @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MaxPQueue' is stored somewhat lazily. In earlier
-- versions of this package, some operations could produce chains of thunks
-- along the spine, occasionally necessitating manual forcing. Now, all
-- operations are careful to force enough to avoid this problem.
{-# DEPRECATED seqSpine "This function is no longer necessary or useful." #-}
seqSpine :: MaxPQueue k a -> b -> b
seqSpine (MaxPQ q) = Q.seqSpine q
