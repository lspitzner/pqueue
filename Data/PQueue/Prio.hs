-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Prio
-- Copyright   :  (c) Henning Thielemann 2017
--                (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue.
-- Each element is associated with a /key/, and the priority queue supports
-- viewing and extracting the element with the top key.
--
-- A worst-case bound is given for each operation.  In some cases, an amortized
-- bound is also specified; these bounds do not hold in a persistent context.
--
-- This implementation is based on a binomial heap augmented with a global root.
-- The spine of the heap is maintained lazily.  To force the spine of the heap,
-- use 'seqSpine'.
--
-- We do not guarantee stable behavior.
-- Ties are broken arbitrarily -- that is, if @k1 <= k2@ and @k2 <= k1@, then there
-- are no guarantees about the relative order in which @k1@, @k2@, and their associated
-- elements are returned.  (Unlike Data.Map, we allow multiple elements with the
-- same key.)
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered.  No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue.Prio (
  PQueue,
  MinPQueue,
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
  -- ** Top view
  findTop,
  getTop,
  deleteTop,
  deleteFindTop,
  adjustTop,
  adjustTopWithKey,
  updateTop,
  updateTopWithKey,
  topView,
  topViewWithKey,
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
  fromOrderedList,
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

import qualified Data.PQueue.Prio.Min as Q
import qualified Data.PQueue.Top as Top
import Data.PQueue.Prio.Private
        (PQueue(PQ), toList, fromOrderedList, empty, union, unions)
import Data.PQueue.Top (Top, Wrap(Wrap, unwrap))

import Control.Applicative (Applicative, (<$>))
import Data.Maybe (fromMaybe)

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null, foldr, foldl)


first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)


type MinPQueue = PQueue Top.Min
type MaxPQueue = PQueue Top.Max


-- | /O(1)/.  Constructs a singleton priority queue.
singleton :: k -> a -> PQueue top k a
singleton k a = PQ (Q.singleton (Wrap k) a)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Inserts
-- an element with the specified key into the queue.
insert :: (Top top, Ord k) => k -> a -> PQueue top k a -> PQueue top k a
insert k a (PQ q) = PQ (Q.insert (Wrap k) a q)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Insert an element into the priority queue,
--   putting it behind elements that compare equal to the inserted one.
insertBehind :: (Top top, Ord k) => k -> a -> PQueue top k a -> PQueue top k a
insertBehind k a (PQ q) = PQ (Q.insertBehind (Wrap k) a q)

-- | /O(1)/.  Checks if this priority queue is empty.
null :: PQueue top k a -> Bool
null (PQ q) = Q.null q

-- | /O(1)/.  Returns the size of this priority queue.
size :: PQueue top k a -> Int
size (PQ q) = Q.size q

-- | /O(1)/.  The top (key, element) in the queue.  Calls 'error' if empty.
findTop :: PQueue top k a -> (k, a)
findTop = fromMaybe (error "Error: findTop called on an empty queue") . getTop

-- | /O(1)/.  The top (key, element) in the queue, if the queue is nonempty.
getTop :: PQueue top k a -> Maybe (k, a)
getTop (PQ q) = do
  (Wrap k, a) <- Q.getMin q
  return (k, a)

-- | /O(log n)/.  Delete and find the element with the top key.  Calls 'error' if empty.
deleteTop :: (Top top, Ord k) => PQueue top k a -> PQueue top k a
deleteTop (PQ q) = PQ (Q.deleteMin q)

-- | /O(log n)/.  Delete and find the element with the top key.  Calls 'error' if empty.
deleteFindTop :: (Top top, Ord k) => PQueue top k a -> ((k, a), PQueue top k a)
deleteFindTop = fromMaybe (error "Error: deleteFindTop called on an empty queue") . topViewWithKey

-- | /O(1)/.  Alter the value at the top key.  If the queue is empty, does nothing.
adjustTop :: (a -> a) -> PQueue top k a -> PQueue top k a
adjustTop = adjustTopWithKey . const

-- | /O(1)/.  Alter the value at the top key.  If the queue is empty, does nothing.
adjustTopWithKey :: (k -> a -> a) -> PQueue top k a -> PQueue top k a
adjustTopWithKey f (PQ q) = PQ (Q.adjustMinWithKey (f . unwrap) q)

-- | /O(log n)/.  (Actually /O(1)/ if there's no deletion.)  Update the value at the top key.
-- If the queue is empty, does nothing.
updateTop :: (Top top, Ord k) => (a -> Maybe a) -> PQueue top k a -> PQueue top k a
updateTop = updateTopWithKey . const

-- | /O(log n)/.  (Actually /O(1)/ if there's no deletion.)  Update the value at the top key.
-- If the queue is empty, does nothing.
updateTopWithKey :: (Top top, Ord k) => (k -> a -> Maybe a) -> PQueue top k a -> PQueue top k a
updateTopWithKey f (PQ q) = PQ (Q.updateMinWithKey (f . unwrap) q)

-- | /O(log n)/.  Retrieves the value associated with the top key of the queue, and the queue
-- stripped of that element, or 'Nothing' if passed an empty queue.
topView :: (Top top, Ord k) => PQueue top k a -> Maybe (a, PQueue top k a)
topView q = do
  ((_, a), q') <- topViewWithKey q
  return (a, q')

-- | /O(log n)/.  Retrieves the top (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
topViewWithKey :: (Top top, Ord k) => PQueue top k a -> Maybe ((k, a), PQueue top k a)
topViewWithKey (PQ q) = do
  ((Wrap k, a), q') <- Q.minViewWithKey q
  return ((k, a), PQ q')

-- | /O(n)/.  Map a function over all values in the queue.
map :: (a -> b) -> PQueue top k a -> PQueue top k b
map = mapWithKey . const

-- | /O(n)/.  Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> PQueue top k a -> PQueue top k b
mapWithKey f (PQ q) = PQ (Q.mapWithKey (f . unwrap) q)

-- | /O(n)/.  Map a function over all values in the queue.
mapKeys :: (Top top, Ord k') => (k -> k') -> PQueue top k a -> PQueue top k' a
mapKeys f (PQ q) = PQ (Q.mapKeys (fmap f) q)

-- | /O(n)/.  @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when @f@ is strictly
-- monotonic.  /The precondition is not checked./  This function has better performance than
-- 'mapKeys'.
mapKeysMonotonic :: (k -> k') -> PQueue top k a -> PQueue top k' a
mapKeysMonotonic f (PQ q) = PQ (Q.mapKeysMonotonic (fmap f) q)

-- | /O(n log n)/.  Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: (Top top, Ord k) => (k -> a -> b -> b) -> b -> PQueue top k a -> b
foldrWithKey f z (PQ q) = Q.foldrWithKey (f . unwrap) z q

-- | /O(n log n)/.  Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: (Top top, Ord k) => (b -> k -> a -> b) -> b -> PQueue top k a -> b
foldlWithKey f z0 (PQ q) = Q.foldlWithKey (\ z -> f z . unwrap) z0 q

-- | /O(n log n)/.  Traverses the elements of the queue in natural order by key.
-- (@'traverseWithKey' f q == 'fromOrderedList' <$> 'traverse' ('uncurry' f) ('toList' q)@)
--
-- If you do not care about the /order/ of the traversal, consider using 'traverseWithKeyU'.
traverseWithKey :: (Top top, Ord k, Applicative f) => (k -> a -> f b) -> PQueue top k a -> f (PQueue top k b)
traverseWithKey f (PQ q) = PQ <$> Q.traverseWithKey (f . unwrap) q

-- | /O(k log n)/.  Takes the first @k@ (key, value) pairs in the queue, or the first @n@ if @k >= n@.
-- (@'take' k q == 'List.take' k ('toList' q)@)
take :: (Top top, Ord k) => Int -> PQueue top k a -> [(k, a)]
take k (PQ q) = fmap (first' unwrap) (Q.take k q)

-- | /O(k log n)/.  Deletes the first @k@ (key, value) pairs in the queue, or returns an empty queue if @k >= n@.
drop :: (Top top, Ord k) => Int -> PQueue top k a -> PQueue top k a
drop k (PQ q) = PQ (Q.drop k q)

-- | /O(k log n)/.  Equivalent to @('take' k q, 'drop' k q)@.
splitAt :: (Top top, Ord k) => Int -> PQueue top k a -> ([(k, a)], PQueue top k a)
splitAt k (PQ q) = case Q.splitAt k q of
  (xs, q') -> (fmap (first' unwrap) xs, PQ q')

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (p . 'snd') ('toList' q)@)
takeWhile :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> [(k, a)]
takeWhile = takeWhileWithKey . const

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (uncurry p) ('toList' q)@)
takeWhileWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> [(k, a)]
takeWhileWithKey p (PQ q) = fmap (first' unwrap) (Q.takeWhileWithKey (p . unwrap) q)

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhile :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> PQueue top k a
dropWhile = dropWhileWithKey . const

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhileWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> PQueue top k a
dropWhileWithKey p (PQ q) = PQ (Q.dropWhileWithKey (p . unwrap) q)

-- | Equivalent to @('takeWhile' p q, 'dropWhile' p q)@.
span :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> ([(k, a)], PQueue top k a)
span = spanWithKey . const

-- | Equivalent to @'span' ('not' . p)@.
break :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> ([(k, a)], PQueue top k a)
break = breakWithKey . const

-- | Equivalent to @'spanWithKey' (\ k a -> 'not' (p k a)) q@.
spanWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> ([(k, a)], PQueue top k a)
spanWithKey p (PQ q) = case Q.spanWithKey (p . unwrap) q of
  (xs, q') -> (fmap (first' unwrap) xs, PQ q')

-- | Equivalent to @'spanWithKey' (\ k a -> 'not' (p k a)) q@.
breakWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> ([(k, a)], PQueue top k a)
breakWithKey p (PQ q) = case Q.breakWithKey (p . unwrap) q of
  (xs, q') -> (fmap (first' unwrap) xs, PQ q')

-- | /O(n)/.  Filter all values that satisfy the predicate.
filter :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> PQueue top k a
filter = filterWithKey . const

-- | /O(n)/.  Filter all values that satisfy the predicate.
filterWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> PQueue top k a
filterWithKey p (PQ q) = PQ (Q.filterWithKey (p . unwrap) q)

-- | /O(n)/.  Partition the queue according to a predicate.  The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partition :: (Top top, Ord k) => (a -> Bool) -> PQueue top k a -> (PQueue top k a, PQueue top k a)
partition = partitionWithKey . const

-- | /O(n)/.  Partition the queue according to a predicate.  The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partitionWithKey :: (Top top, Ord k) => (k -> a -> Bool) -> PQueue top k a -> (PQueue top k a, PQueue top k a)
partitionWithKey p (PQ q) = case Q.partitionWithKey (p . unwrap) q of
  (q1, q0) -> (PQ q1, PQ q0)

-- | /O(n)/.  Map values and collect the 'Just' results.
mapMaybe :: (Top top, Ord k) => (a -> Maybe b) -> PQueue top k a -> PQueue top k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n)/.  Map values and collect the 'Just' results.
mapMaybeWithKey :: (Top top, Ord k) => (k -> a -> Maybe b) -> PQueue top k a -> PQueue top k b
mapMaybeWithKey f (PQ q) = PQ (Q.mapMaybeWithKey (f . unwrap) q)

-- | /O(n)/.  Map values and separate the 'Left' and 'Right' results.
mapEither :: (Top top, Ord k) => (a -> Either b c) -> PQueue top k a -> (PQueue top k b, PQueue top k c)
mapEither = mapEitherWithKey . const

-- | /O(n)/.  Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: (Top top, Ord k) => (k -> a -> Either b c) -> PQueue top k a -> (PQueue top k b, PQueue top k c)
mapEitherWithKey f (PQ q) = case Q.mapEitherWithKey (f . unwrap) q of
  (qL, qR) -> (PQ qL, PQ qR)

-- | /O(n)/.  Build a priority queue from the list of (key, value) pairs.
fromList :: (Top top, Ord k) => [(k, a)] -> PQueue top k a
fromList = PQ . Q.fromList . fmap (first' Wrap)

newtype
  FromList k a top =
    FromList {runFromList :: [(Wrap top k, a)] -> Q.MinPQueue (Wrap top k) a}

-- | /O(n)/.  Build a priority queue from an ascending list of (key, value) pairs.  /The precondition is not checked./
fromAscList :: (Top top) => [(k, a)] -> PQueue top k a
fromAscList =
  PQ .
  runFromList (Top.switch (FromList Q.fromAscList) (FromList Q.fromDescList)) .
  fmap (first' Wrap)

-- | /O(n)/.  Build a priority queue from a descending list of (key, value) pairs.  /The precondition is not checked./
fromDescList :: (Top top) => [(k, a)] -> PQueue top k a
fromDescList =
  PQ .
  runFromList (Top.switch (FromList Q.fromDescList) (FromList Q.fromAscList)) .
  fmap (first' Wrap)

-- | /O(n log n)/.  Return all keys of the queue in natural order, that is, top keys first.
keys :: (Top top, Ord k) => PQueue top k a -> [k]
keys = fmap fst . toList

-- | /O(n log n)/.  Return all elements of the queue in natural order by key.
elems :: (Top top, Ord k) => PQueue top k a -> [a]
elems = fmap snd . toList

-- | /O(n log n)/.  Equivalent to 'toList'.
assocs :: (Top top, Ord k) => PQueue top k a -> [(k, a)]
assocs = toList

newtype
  ToList k a top =
    ToList {runToList :: Q.MinPQueue (Wrap top k) a -> [(Wrap top k, a)]}

-- | /O(n log n)/.  Return all (key, value) pairs in ascending order by key.
toAscList :: (Top top, Ord k) => PQueue top k a -> [(k, a)]
toAscList (PQ q) =
  fmap (first' unwrap) $
  runToList (Top.switch (ToList Q.toAscList) (ToList Q.toDescList)) q

-- | /O(n log n)/.  Return all (key, value) pairs in descending order by key.
toDescList :: (Top top, Ord k) => PQueue top k a -> [(k, a)]
toDescList (PQ q) =
  fmap (first' unwrap) $
  runToList (Top.switch (ToList Q.toDescList) (ToList Q.toAscList)) q

-- | /O(n)/.  An unordered right fold over the elements of the queue, in no particular order.
foldrU :: (a -> b -> b) -> b -> PQueue top k a -> b
foldrU = foldrWithKeyU . const

-- | /O(n)/.  An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> PQueue top k a -> b
foldrWithKeyU f z (PQ q) = Q.foldrWithKeyU (f . unwrap) z q

-- | /O(n)/.  An unordered left fold over the elements of the queue, in no particular order.
foldlU :: (b -> a -> b) -> b -> PQueue top k a -> b
foldlU f = foldlWithKeyU (const . f)

-- | /O(n)/.  An unordered left fold over the elements of the queue, in no particular order.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> PQueue top k a -> b
foldlWithKeyU f z0 (PQ q) = Q.foldlWithKeyU (\ z -> f z . unwrap) z0 q

-- | /O(n)/.  An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseU :: (Applicative f) => (a -> f b) -> PQueue top k a -> f (PQueue top k b)
traverseU = traverseWithKeyU . const

-- | /O(n)/.  An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseWithKeyU :: (Applicative f) => (k -> a -> f b) -> PQueue top k a -> f (PQueue top k b)
traverseWithKeyU f (PQ q) = PQ <$> Q.traverseWithKeyU (f . unwrap) q

-- | /O(n)/.  Return all keys of the queue in no particular order.
keysU :: PQueue top k a -> [k]
keysU = fmap fst . toListU

-- | /O(n)/.  Return all elements of the queue in no particular order.
elemsU :: PQueue top k a -> [a]
elemsU = fmap snd . toListU

-- | /O(n)/.  Equivalent to 'toListU'.
assocsU :: PQueue top k a -> [(k, a)]
assocsU = toListU

-- | /O(n)/.  Returns all (key, value) pairs in the queue in no particular order.
toListU :: PQueue top k a -> [(k, a)]
toListU (PQ q) = fmap (first' unwrap) (Q.toListU q)

-- | /O(log n)/.  Analogous to @deepseq@ in the @deepseq@ package, but only forces the spine of the binomial heap.
seqSpine :: PQueue top k a -> b -> b
seqSpine (PQ q) = Q.seqSpine q
