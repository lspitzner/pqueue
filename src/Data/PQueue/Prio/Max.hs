{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Prio.Max
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue.
-- Each element is associated with a /key/, and the priority queue supports
-- viewing and extracting the element with the maximum key.
--
-- A worst-case bound is given for each operation. In some cases, an amortized
-- bound is also specified; these bounds do not hold in a persistent context.
--
-- This implementation is based on a binomial heap augmented with a global root.
-- The spine of the heap is maintained lazily. To force the spine of the heap,
-- use 'seqSpine'.
--
-- We do not guarantee stable behavior.
-- Ties are broken arbitrarily -- that is, if @k1 <= k2@ and @k2 <= k1@, then there
-- are no guarantees about the relative order in which @k1@, @k2@, and their associated
-- elements are returned. (Unlike Data.Map, we allow multiple elements with the
-- same key.)
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered. No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue.Prio.Max (
  MaxPQueue,
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
  adjustMaxWithKey,
  updateMax,
  updateMaxWithKey,
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
  foldrWithKeyU,
  foldlU,
  foldlWithKeyU,
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

import Control.Applicative (Applicative, (<$>))
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.Traversable (Traversable(traverse))
import Data.Foldable (Foldable, foldr, foldl)
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Max.Internals

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null, foldr, foldl)

import qualified Data.PQueue.Prio.Min as Q

#ifdef __GLASGOW_HASKELL__
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)

#if MIN_VERSION_base(4,9,0)
instance Ord k => Semigroup (MaxPQueue k a) where
  (<>) = union
#endif

instance Ord k => Monoid (MaxPQueue k a) where
  mempty = empty
  mappend = union
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

instance Ord k => Foldable (MaxPQueue k) where
  foldr f z (MaxPQ q) = foldr f z q
  foldl f z (MaxPQ q) = foldl f z q

instance Ord k => Traversable (MaxPQueue k) where
  traverse f (MaxPQ q) = MaxPQ <$> traverse f q

-- | /O(1)/. Returns the empty priority queue.
empty :: MaxPQueue k a
empty = MaxPQ Q.empty

-- | /O(1)/. Constructs a singleton priority queue.
singleton :: k -> a -> MaxPQueue k a
singleton k a = MaxPQ (Q.singleton (Down k) a)

-- | Amortized /O(1)/, worst-case /O(log n)/. Inserts
-- an element with the specified key into the queue.
insert :: Ord k => k -> a -> MaxPQueue k a -> MaxPQueue k a
insert k a (MaxPQ q) = MaxPQ (Q.insert (Down k) a q)

-- | /O(n)/ (an earlier implementation had /O(1)/ but was buggy).
-- Insert an element with the specified key into the priority queue,
-- putting it behind elements whose key compares equal to the
-- inserted one.
insertBehind :: Ord k => k -> a -> MaxPQueue k a -> MaxPQueue k a
insertBehind k a (MaxPQ q) = MaxPQ (Q.insertBehind (Down k) a q)

-- | Amortized /O(log(min(n1, n2)))/, worst-case /O(log(max(n1, n2)))/. Returns the union
-- of the two specified queues.
union :: Ord k => MaxPQueue k a -> MaxPQueue k a -> MaxPQueue k a
MaxPQ q1 `union` MaxPQ q2 = MaxPQ (q1 `Q.union` q2)

-- | The union of a list of queues: (@'unions' == 'List.foldl' 'union' 'empty'@).
unions :: Ord k => [MaxPQueue k a] -> MaxPQueue k a
unions qs = MaxPQ (Q.unions [q | MaxPQ q <- qs])

-- | /O(1)/. Checks if this priority queue is empty.
null :: MaxPQueue k a -> Bool
null (MaxPQ q) = Q.null q

-- | /O(1)/. Returns the size of this priority queue.
size :: MaxPQueue k a -> Int
size (MaxPQ q) = Q.size q

-- | /O(1)/. The maximal (key, element) in the queue. Calls 'error' if empty.
findMax :: MaxPQueue k a -> (k, a)
findMax = fromMaybe (error "Error: findMax called on an empty queue") . getMax

-- | /O(1)/. The maximal (key, element) in the queue, if the queue is nonempty.
getMax :: MaxPQueue k a -> Maybe (k, a)
getMax (MaxPQ q) = do
  (Down k, a) <- Q.getMin q
  return (k, a)

-- | /O(log n)/. Delete and find the element with the maximum key. Calls 'error' if empty.
deleteMax :: Ord k => MaxPQueue k a -> MaxPQueue k a
deleteMax (MaxPQ q) = MaxPQ (Q.deleteMin q)

-- | /O(log n)/. Delete and find the element with the maximum key. Calls 'error' if empty.
deleteFindMax :: Ord k => MaxPQueue k a -> ((k, a), MaxPQueue k a)
deleteFindMax = fromMaybe (error "Error: deleteFindMax called on an empty queue") . maxViewWithKey

-- | /O(1)/. Alter the value at the maximum key. If the queue is empty, does nothing.
adjustMax :: (a -> a) -> MaxPQueue k a -> MaxPQueue k a
adjustMax = adjustMaxWithKey . const

-- | /O(1)/. Alter the value at the maximum key. If the queue is empty, does nothing.
adjustMaxWithKey :: (k -> a -> a) -> MaxPQueue k a -> MaxPQueue k a
adjustMaxWithKey f (MaxPQ q) = MaxPQ (Q.adjustMinWithKey (f . unDown) q)

-- | /O(log n)/. (Actually /O(1)/ if there's no deletion.) Update the value at the maximum key.
-- If the queue is empty, does nothing.
updateMax :: Ord k => (a -> Maybe a) -> MaxPQueue k a -> MaxPQueue k a
updateMax = updateMaxWithKey . const

-- | /O(log n)/. (Actually /O(1)/ if there's no deletion.) Update the value at the maximum key.
-- If the queue is empty, does nothing.
updateMaxWithKey :: Ord k => (k -> a -> Maybe a) -> MaxPQueue k a -> MaxPQueue k a
updateMaxWithKey f (MaxPQ q) = MaxPQ (Q.updateMinWithKey (f . unDown) q)

-- | /O(log n)/. Retrieves the value associated with the maximum key of the queue, and the queue
-- stripped of that element, or 'Nothing' if passed an empty queue.
maxView :: Ord k => MaxPQueue k a -> Maybe (a, MaxPQueue k a)
maxView q = do
  ((_, a), q') <- maxViewWithKey q
  return (a, q')

-- | /O(log n)/. Retrieves the maximal (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
maxViewWithKey :: Ord k => MaxPQueue k a -> Maybe ((k, a), MaxPQueue k a)
maxViewWithKey (MaxPQ q) = do
  ((Down k, a), q') <- Q.minViewWithKey q
  return ((k, a), MaxPQ q')

-- | /O(n)/. Map a function over all values in the queue.
map :: (a -> b) -> MaxPQueue k a -> MaxPQueue k b
map = mapWithKey . const

-- | /O(n)/. Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> MaxPQueue k a -> MaxPQueue k b
mapWithKey f (MaxPQ q) = MaxPQ (Q.mapWithKey (f . unDown) q)

-- | /O(n)/. Map a function over all values in the queue.
mapKeys :: Ord k' => (k -> k') -> MaxPQueue k a -> MaxPQueue k' a
mapKeys f (MaxPQ q) = MaxPQ (Q.mapKeys (fmap f) q)

-- | /O(n)/. @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when @f@ is strictly
-- monotonic. /The precondition is not checked./  This function has better performance than
-- 'mapKeys'.
mapKeysMonotonic :: (k -> k') -> MaxPQueue k a -> MaxPQueue k' a
mapKeysMonotonic f (MaxPQ q) = MaxPQ (Q.mapKeysMonotonic (fmap f) q)

-- | /O(n log n)/. Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toDescList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> MaxPQueue k a -> b
foldrWithKey f z (MaxPQ q) = Q.foldrWithKey (f . unDown) z q

-- | /O(n log n)/. Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toDescList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: Ord k => (b -> k -> a -> b) -> b -> MaxPQueue k a -> b
foldlWithKey f z0 (MaxPQ q) = Q.foldlWithKey (\z -> f z . unDown) z0 q

-- | /O(n log n)/. Traverses the elements of the queue in descending order by key.
-- (@'traverseWithKey' f q == 'fromDescList' <$> 'traverse' ('uncurry' f) ('toDescList' q)@)
--
-- If you do not care about the /order/ of the traversal, consider using 'traverseWithKeyU'.
traverseWithKey :: (Ord k, Applicative f) => (k -> a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseWithKey f (MaxPQ q) = MaxPQ <$> Q.traverseWithKey (f . unDown) q

-- | /O(k log n)/. Takes the first @k@ (key, value) pairs in the queue, or the first @n@ if @k >= n@.
-- (@'take' k q == 'List.take' k ('toDescList' q)@)
take :: Ord k => Int -> MaxPQueue k a -> [(k, a)]
take k (MaxPQ q) = fmap (first' unDown) (Q.take k q)

-- | /O(k log n)/. Deletes the first @k@ (key, value) pairs in the queue, or returns an empty queue if @k >= n@.
drop :: Ord k => Int -> MaxPQueue k a -> MaxPQueue k a
drop k (MaxPQ q) = MaxPQ (Q.drop k q)

-- | /O(k log n)/. Equivalent to @('take' k q, 'drop' k q)@.
splitAt :: Ord k => Int -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
splitAt k (MaxPQ q) = case Q.splitAt k q of
  (xs, q') -> (fmap (first' unDown) xs, MaxPQ q')

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (p . 'snd') ('toDescList' q)@)
takeWhile :: Ord k => (a -> Bool) -> MaxPQueue k a -> [(k, a)]
takeWhile = takeWhileWithKey . const

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (uncurry p) ('toDescList' q)@)
takeWhileWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> [(k, a)]
takeWhileWithKey p (MaxPQ q) = fmap (first' unDown) (Q.takeWhileWithKey (p . unDown) q)

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhile :: Ord k => (a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
dropWhile = dropWhileWithKey . const

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhileWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
dropWhileWithKey p (MaxPQ q) = MaxPQ (Q.dropWhileWithKey (p . unDown) q)

-- | Equivalent to @('takeWhile' p q, 'dropWhile' p q)@.
span :: Ord k => (a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
span = spanWithKey . const

-- | Equivalent to @'span' ('not' . p)@.
break :: Ord k => (a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
break = breakWithKey . const

-- | Equivalent to @'spanWithKey' (\k a -> 'not' (p k a)) q@.
spanWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
spanWithKey p (MaxPQ q) = case Q.spanWithKey (p . unDown) q of
  (xs, q') -> (fmap (first' unDown) xs, MaxPQ q')

-- | Equivalent to @'spanWithKey' (\k a -> 'not' (p k a)) q@.
breakWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> ([(k, a)], MaxPQueue k a)
breakWithKey p (MaxPQ q) = case Q.breakWithKey (p . unDown) q of
  (xs, q') -> (fmap (first' unDown) xs, MaxPQ q')

-- | /O(n)/. Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
filter = filterWithKey . const

-- | /O(n)/. Filter all values that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> MaxPQueue k a
filterWithKey p (MaxPQ q) = MaxPQ (Q.filterWithKey (p . unDown) q)

-- | /O(n)/. Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partition :: Ord k => (a -> Bool) -> MaxPQueue k a -> (MaxPQueue k a, MaxPQueue k a)
partition = partitionWithKey . const

-- | /O(n)/. Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> MaxPQueue k a -> (MaxPQueue k a, MaxPQueue k a)
partitionWithKey p (MaxPQ q) = case Q.partitionWithKey (p . unDown) q of
  (q1, q0) -> (MaxPQ q1, MaxPQ q0)

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> MaxPQueue k a -> MaxPQueue k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> MaxPQueue k a -> MaxPQueue k b
mapMaybeWithKey f (MaxPQ q) = MaxPQ (Q.mapMaybeWithKey (f . unDown) q)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> MaxPQueue k a -> (MaxPQueue k b, MaxPQueue k c)
mapEither = mapEitherWithKey . const

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> MaxPQueue k a -> (MaxPQueue k b, MaxPQueue k c)
mapEitherWithKey f (MaxPQ q) = case Q.mapEitherWithKey (f . unDown) q of
  (qL, qR) -> (MaxPQ qL, MaxPQ qR)

-- | /O(n)/. Build a priority queue from the list of (key, value) pairs.
fromList :: Ord k => [(k, a)] -> MaxPQueue k a
fromList = MaxPQ . Q.fromList . fmap (first' Down)

-- | /O(n)/. Build a priority queue from an ascending list of (key, value) pairs. /The precondition is not checked./
fromAscList :: [(k, a)] -> MaxPQueue k a
fromAscList = MaxPQ . Q.fromDescList . fmap (first' Down)

-- | /O(n)/. Build a priority queue from a descending list of (key, value) pairs. /The precondition is not checked./
fromDescList :: [(k, a)] -> MaxPQueue k a
fromDescList = MaxPQ . Q.fromAscList . fmap (first' Down)

-- | /O(n log n)/. Return all keys of the queue in descending order.
keys :: Ord k => MaxPQueue k a -> [k]
keys = fmap fst . toDescList

-- | /O(n log n)/. Return all elements of the queue in descending order by key.
elems :: Ord k => MaxPQueue k a -> [a]
elems = fmap snd . toDescList

-- | /O(n log n)/. Equivalent to 'toDescList'.
assocs :: Ord k => MaxPQueue k a -> [(k, a)]
assocs = toDescList

-- | /O(n log n)/. Return all (key, value) pairs in ascending order by key.
toAscList :: Ord k => MaxPQueue k a -> [(k, a)]
toAscList (MaxPQ q) = fmap (first' unDown) (Q.toDescList q)

-- | /O(n log n)/. Return all (key, value) pairs in descending order by key.
toDescList :: Ord k => MaxPQueue k a -> [(k, a)]
toDescList (MaxPQ q) = fmap (first' unDown) (Q.toAscList q)

-- | /O(n log n)/. Equivalent to 'toDescList'.
--
-- If the traversal order is irrelevant, consider using 'toListU'.
toList :: Ord k => MaxPQueue k a -> [(k, a)]
toList = toDescList

-- | /O(n)/. An unordered right fold over the elements of the queue, in no particular order.
foldrU :: (a -> b -> b) -> b -> MaxPQueue k a -> b
foldrU = foldrWithKeyU . const

-- | /O(n)/. An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> MaxPQueue k a -> b
foldrWithKeyU f z (MaxPQ q) = Q.foldrWithKeyU (f . unDown) z q

-- | /O(n)/. An unordered left fold over the elements of the queue, in no particular order.
foldlU :: (b -> a -> b) -> b -> MaxPQueue k a -> b
foldlU f = foldlWithKeyU (const . f)

-- | /O(n)/. An unordered left fold over the elements of the queue, in no particular order.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> MaxPQueue k a -> b
foldlWithKeyU f z0 (MaxPQ q) = Q.foldlWithKeyU (\z -> f z . unDown) z0 q

-- | /O(n)/. An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseU :: (Applicative f) => (a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseU = traverseWithKeyU . const

-- | /O(n)/. An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseWithKeyU :: (Applicative f) => (k -> a -> f b) -> MaxPQueue k a -> f (MaxPQueue k b)
traverseWithKeyU f (MaxPQ q) = MaxPQ <$> Q.traverseWithKeyU (f . unDown) q

-- | /O(n)/. Return all keys of the queue in no particular order.
keysU :: MaxPQueue k a -> [k]
keysU = fmap fst . toListU

-- | /O(n)/. Return all elements of the queue in no particular order.
elemsU :: MaxPQueue k a -> [a]
elemsU = fmap snd . toListU

-- | /O(n)/. Equivalent to 'toListU'.
assocsU :: MaxPQueue k a -> [(k, a)]
assocsU = toListU

-- | /O(n)/. Returns all (key, value) pairs in the queue in no particular order.
toListU :: MaxPQueue k a -> [(k, a)]
toListU (MaxPQ q) = fmap (first' unDown) (Q.toListU q)

-- | /O(log n)/. Analogous to @deepseq@ in the @deepseq@ package, but only forces the spine of the binomial heap.
seqSpine :: MaxPQueue k a -> b -> b
seqSpine (MaxPQ q) = Q.seqSpine q
