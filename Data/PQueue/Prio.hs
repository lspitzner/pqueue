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
-- viewing and extracting the element with the first key.
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
  -- ** First view
  findFirst,
  getFirst,
  deleteFirst,
  deleteFindFirst,
  adjustFirst,
  adjustFirstWithKey,
  updateFirst,
  updateFirstWithKey,
  firstView,
  firstViewWithKey,
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
import Data.PQueue.Top (First, TaggedF(TaggedF, unTaggedF))

import Control.Applicative (Applicative, (<$>))
import Data.Maybe (fromMaybe)

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null, foldr, foldl)


first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)


type MinPQueue = PQueue Top.Min
type MaxPQueue = PQueue Top.Max


-- | /O(1)/.  Constructs a singleton priority queue.
singleton :: k -> a -> PQueue first k a
singleton k a = PQ (Q.singleton (TaggedF k) a)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Inserts
-- an element with the specified key into the queue.
insert :: (First first, Ord k) => k -> a -> PQueue first k a -> PQueue first k a
insert k a (PQ q) = PQ (Q.insert (TaggedF k) a q)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Insert an element into the priority queue,
--   putting it behind elements that compare equal to the inserted one.
insertBehind :: (First first, Ord k) => k -> a -> PQueue first k a -> PQueue first k a
insertBehind k a (PQ q) = PQ (Q.insertBehind (TaggedF k) a q)

-- | /O(1)/.  Checks if this priority queue is empty.
null :: PQueue first k a -> Bool
null (PQ q) = Q.null q

-- | /O(1)/.  Returns the size of this priority queue.
size :: PQueue first k a -> Int
size (PQ q) = Q.size q

-- | /O(1)/.  The first (key, element) in the queue.  Calls 'error' if empty.
findFirst :: PQueue first k a -> (k, a)
findFirst = fromMaybe (error "Error: findFirst called on an empty queue") . getFirst

-- | /O(1)/.  The first (key, element) in the queue, if the queue is nonempty.
getFirst :: PQueue first k a -> Maybe (k, a)
getFirst (PQ q) = do
  (TaggedF k, a) <- Q.getMin q
  return (k, a)

-- | /O(log n)/.  Delete and find the element with the first key.  Calls 'error' if empty.
deleteFirst :: (First first, Ord k) => PQueue first k a -> PQueue first k a
deleteFirst (PQ q) = PQ (Q.deleteMin q)

-- | /O(log n)/.  Delete and find the element with the first key.  Calls 'error' if empty.
deleteFindFirst :: (First first, Ord k) => PQueue first k a -> ((k, a), PQueue first k a)
deleteFindFirst = fromMaybe (error "Error: deleteFindFirst called on an empty queue") . firstViewWithKey

-- | /O(1)/.  Alter the value at the first key.  If the queue is empty, does nothing.
adjustFirst :: (a -> a) -> PQueue first k a -> PQueue first k a
adjustFirst = adjustFirstWithKey . const

-- | /O(1)/.  Alter the value at the first key.  If the queue is empty, does nothing.
adjustFirstWithKey :: (k -> a -> a) -> PQueue first k a -> PQueue first k a
adjustFirstWithKey f (PQ q) = PQ (Q.adjustMinWithKey (f . unTaggedF) q)

-- | /O(log n)/.  (Actually /O(1)/ if there's no deletion.)  Update the value at the first key.
-- If the queue is empty, does nothing.
updateFirst :: (First first, Ord k) => (a -> Maybe a) -> PQueue first k a -> PQueue first k a
updateFirst = updateFirstWithKey . const

-- | /O(log n)/.  (Actually /O(1)/ if there's no deletion.)  Update the value at the first key.
-- If the queue is empty, does nothing.
updateFirstWithKey :: (First first, Ord k) => (k -> a -> Maybe a) -> PQueue first k a -> PQueue first k a
updateFirstWithKey f (PQ q) = PQ (Q.updateMinWithKey (f . unTaggedF) q)

-- | /O(log n)/.  Retrieves the value associated with the first key of the queue, and the queue
-- stripped of that element, or 'Nothing' if passed an empty queue.
firstView :: (First first, Ord k) => PQueue first k a -> Maybe (a, PQueue first k a)
firstView q = do
  ((_, a), q') <- firstViewWithKey q
  return (a, q')

-- | /O(log n)/.  Retrieves the first (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
firstViewWithKey :: (First first, Ord k) => PQueue first k a -> Maybe ((k, a), PQueue first k a)
firstViewWithKey (PQ q) = do
  ((TaggedF k, a), q') <- Q.minViewWithKey q
  return ((k, a), PQ q')

-- | /O(n)/.  Map a function over all values in the queue.
map :: (a -> b) -> PQueue first k a -> PQueue first k b
map = mapWithKey . const

-- | /O(n)/.  Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> PQueue first k a -> PQueue first k b
mapWithKey f (PQ q) = PQ (Q.mapWithKey (f . unTaggedF) q)

-- | /O(n)/.  Map a function over all values in the queue.
mapKeys :: (First first, Ord k') => (k -> k') -> PQueue first k a -> PQueue first k' a
mapKeys f (PQ q) = PQ (Q.mapKeys (fmap f) q)

-- | /O(n)/.  @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when @f@ is strictly
-- monotonic.  /The precondition is not checked./  This function has better performance than
-- 'mapKeys'.
mapKeysMonotonic :: (k -> k') -> PQueue first k a -> PQueue first k' a
mapKeysMonotonic f (PQ q) = PQ (Q.mapKeysMonotonic (fmap f) q)

-- | /O(n log n)/.  Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: (First first, Ord k) => (k -> a -> b -> b) -> b -> PQueue first k a -> b
foldrWithKey f z (PQ q) = Q.foldrWithKey (f . unTaggedF) z q

-- | /O(n log n)/.  Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: (First first, Ord k) => (b -> k -> a -> b) -> b -> PQueue first k a -> b
foldlWithKey f z0 (PQ q) = Q.foldlWithKey (\ z -> f z . unTaggedF) z0 q

-- | /O(n log n)/.  Traverses the elements of the queue in natural order by key.
-- (@'traverseWithKey' f q == 'fromOrderedList' <$> 'traverse' ('uncurry' f) ('toList' q)@)
--
-- If you do not care about the /order/ of the traversal, consider using 'traverseWithKeyU'.
traverseWithKey :: (First first, Ord k, Applicative f) => (k -> a -> f b) -> PQueue first k a -> f (PQueue first k b)
traverseWithKey f (PQ q) = PQ <$> Q.traverseWithKey (f . unTaggedF) q

-- | /O(k log n)/.  Takes the first @k@ (key, value) pairs in the queue, or the first @n@ if @k >= n@.
-- (@'take' k q == 'List.take' k ('toList' q)@)
take :: (First first, Ord k) => Int -> PQueue first k a -> [(k, a)]
take k (PQ q) = fmap (first' unTaggedF) (Q.take k q)

-- | /O(k log n)/.  Deletes the first @k@ (key, value) pairs in the queue, or returns an empty queue if @k >= n@.
drop :: (First first, Ord k) => Int -> PQueue first k a -> PQueue first k a
drop k (PQ q) = PQ (Q.drop k q)

-- | /O(k log n)/.  Equivalent to @('take' k q, 'drop' k q)@.
splitAt :: (First first, Ord k) => Int -> PQueue first k a -> ([(k, a)], PQueue first k a)
splitAt k (PQ q) = case Q.splitAt k q of
  (xs, q') -> (fmap (first' unTaggedF) xs, PQ q')

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (p . 'snd') ('toList' q)@)
takeWhile :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> [(k, a)]
takeWhile = takeWhileWithKey . const

-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (uncurry p) ('toList' q)@)
takeWhileWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> [(k, a)]
takeWhileWithKey p (PQ q) = fmap (first' unTaggedF) (Q.takeWhileWithKey (p . unTaggedF) q)

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhile :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> PQueue first k a
dropWhile = dropWhileWithKey . const

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhileWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> PQueue first k a
dropWhileWithKey p (PQ q) = PQ (Q.dropWhileWithKey (p . unTaggedF) q)

-- | Equivalent to @('takeWhile' p q, 'dropWhile' p q)@.
span :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> ([(k, a)], PQueue first k a)
span = spanWithKey . const

-- | Equivalent to @'span' ('not' . p)@.
break :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> ([(k, a)], PQueue first k a)
break = breakWithKey . const

-- | Equivalent to @'spanWithKey' (\ k a -> 'not' (p k a)) q@.
spanWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> ([(k, a)], PQueue first k a)
spanWithKey p (PQ q) = case Q.spanWithKey (p . unTaggedF) q of
  (xs, q') -> (fmap (first' unTaggedF) xs, PQ q')

-- | Equivalent to @'spanWithKey' (\ k a -> 'not' (p k a)) q@.
breakWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> ([(k, a)], PQueue first k a)
breakWithKey p (PQ q) = case Q.breakWithKey (p . unTaggedF) q of
  (xs, q') -> (fmap (first' unTaggedF) xs, PQ q')

-- | /O(n)/.  Filter all values that satisfy the predicate.
filter :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> PQueue first k a
filter = filterWithKey . const

-- | /O(n)/.  Filter all values that satisfy the predicate.
filterWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> PQueue first k a
filterWithKey p (PQ q) = PQ (Q.filterWithKey (p . unTaggedF) q)

-- | /O(n)/.  Partition the queue according to a predicate.  The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partition :: (First first, Ord k) => (a -> Bool) -> PQueue first k a -> (PQueue first k a, PQueue first k a)
partition = partitionWithKey . const

-- | /O(n)/.  Partition the queue according to a predicate.  The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partitionWithKey :: (First first, Ord k) => (k -> a -> Bool) -> PQueue first k a -> (PQueue first k a, PQueue first k a)
partitionWithKey p (PQ q) = case Q.partitionWithKey (p . unTaggedF) q of
  (q1, q0) -> (PQ q1, PQ q0)

-- | /O(n)/.  Map values and collect the 'Just' results.
mapMaybe :: (First first, Ord k) => (a -> Maybe b) -> PQueue first k a -> PQueue first k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n)/.  Map values and collect the 'Just' results.
mapMaybeWithKey :: (First first, Ord k) => (k -> a -> Maybe b) -> PQueue first k a -> PQueue first k b
mapMaybeWithKey f (PQ q) = PQ (Q.mapMaybeWithKey (f . unTaggedF) q)

-- | /O(n)/.  Map values and separate the 'Left' and 'Right' results.
mapEither :: (First first, Ord k) => (a -> Either b c) -> PQueue first k a -> (PQueue first k b, PQueue first k c)
mapEither = mapEitherWithKey . const

-- | /O(n)/.  Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: (First first, Ord k) => (k -> a -> Either b c) -> PQueue first k a -> (PQueue first k b, PQueue first k c)
mapEitherWithKey f (PQ q) = case Q.mapEitherWithKey (f . unTaggedF) q of
  (qL, qR) -> (PQ qL, PQ qR)

-- | /O(n)/.  Build a priority queue from the list of (key, value) pairs.
fromList :: (First first, Ord k) => [(k, a)] -> PQueue first k a
fromList = PQ . Q.fromList . fmap (first' TaggedF)

newtype
  FromList k a first =
    FromList {runFromList :: [(TaggedF first k, a)] -> Q.MinPQueue (TaggedF first k) a}

-- | /O(n)/.  Build a priority queue from an ascending list of (key, value) pairs.  /The precondition is not checked./
fromAscList :: (First first) => [(k, a)] -> PQueue first k a
fromAscList =
  PQ .
  runFromList (Top.switch (FromList Q.fromAscList) (FromList Q.fromDescList)) .
  fmap (first' TaggedF)

-- | /O(n)/.  Build a priority queue from a descending list of (key, value) pairs.  /The precondition is not checked./
fromDescList :: (First first) => [(k, a)] -> PQueue first k a
fromDescList =
  PQ .
  runFromList (Top.switch (FromList Q.fromDescList) (FromList Q.fromAscList)) .
  fmap (first' TaggedF)

-- | /O(n log n)/.  Return all keys of the queue in natural order, that is, first keys first.
keys :: (First first, Ord k) => PQueue first k a -> [k]
keys = fmap fst . toList

-- | /O(n log n)/.  Return all elements of the queue in natural order by key.
elems :: (First first, Ord k) => PQueue first k a -> [a]
elems = fmap snd . toList

-- | /O(n log n)/.  Equivalent to 'toList'.
assocs :: (First first, Ord k) => PQueue first k a -> [(k, a)]
assocs = toList

newtype
  ToList k a first =
    ToList {runToList :: Q.MinPQueue (TaggedF first k) a -> [(TaggedF first k, a)]}

-- | /O(n log n)/.  Return all (key, value) pairs in ascending order by key.
toAscList :: (First first, Ord k) => PQueue first k a -> [(k, a)]
toAscList (PQ q) =
  fmap (first' unTaggedF) $
  runToList (Top.switch (ToList Q.toAscList) (ToList Q.toDescList)) q

-- | /O(n log n)/.  Return all (key, value) pairs in descending order by key.
toDescList :: (First first, Ord k) => PQueue first k a -> [(k, a)]
toDescList (PQ q) =
  fmap (first' unTaggedF) $
  runToList (Top.switch (ToList Q.toDescList) (ToList Q.toAscList)) q

-- | /O(n)/.  An unordered right fold over the elements of the queue, in no particular order.
foldrU :: (a -> b -> b) -> b -> PQueue first k a -> b
foldrU = foldrWithKeyU . const

-- | /O(n)/.  An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> PQueue first k a -> b
foldrWithKeyU f z (PQ q) = Q.foldrWithKeyU (f . unTaggedF) z q

-- | /O(n)/.  An unordered left fold over the elements of the queue, in no particular order.
foldlU :: (b -> a -> b) -> b -> PQueue first k a -> b
foldlU f = foldlWithKeyU (const . f)

-- | /O(n)/.  An unordered left fold over the elements of the queue, in no particular order.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> PQueue first k a -> b
foldlWithKeyU f z0 (PQ q) = Q.foldlWithKeyU (\ z -> f z . unTaggedF) z0 q

-- | /O(n)/.  An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseU :: (Applicative f) => (a -> f b) -> PQueue first k a -> f (PQueue first k b)
traverseU = traverseWithKeyU . const

-- | /O(n)/.  An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseWithKeyU :: (Applicative f) => (k -> a -> f b) -> PQueue first k a -> f (PQueue first k b)
traverseWithKeyU f (PQ q) = PQ <$> Q.traverseWithKeyU (f . unTaggedF) q

-- | /O(n)/.  Return all keys of the queue in no particular order.
keysU :: PQueue first k a -> [k]
keysU = fmap fst . toListU

-- | /O(n)/.  Return all elements of the queue in no particular order.
elemsU :: PQueue first k a -> [a]
elemsU = fmap snd . toListU

-- | /O(n)/.  Equivalent to 'toListU'.
assocsU :: PQueue first k a -> [(k, a)]
assocsU = toListU

-- | /O(n)/.  Returns all (key, value) pairs in the queue in no particular order.
toListU :: PQueue first k a -> [(k, a)]
toListU (PQ q) = fmap (first' unTaggedF) (Q.toListU q)

-- | /O(log n)/.  Analogous to @deepseq@ in the @deepseq@ package, but only forces the spine of the binomial heap.
seqSpine :: PQueue first k a -> b -> b
seqSpine (PQ q) = Q.seqSpine q
