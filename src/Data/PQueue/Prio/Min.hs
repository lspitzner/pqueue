{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Prio.Min
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue.
-- Each element is associated with a /key/, and the priority queue supports
-- viewing and extracting the element with the minimum key.
--
-- A worst-case bound is given for each operation. In some cases, an amortized
-- bound is also specified; these bounds hold even in a persistent context.
--
-- This implementation is based on a binomial heap augmented with a global root.
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
module Data.PQueue.Prio.Min (
#if __GLASGOW_HASKELL__ >= 802
  MinPQueue (Data.PQueue.Prio.Min.Empty, (:<)),
#elif defined (__GLASGOW_HASKELL__)
  MinPQueue,
  pattern Data.PQueue.Prio.Min.Empty,
  pattern (:<),
#endif
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
  -- ** Minimum view
  findMin,
  getMin,
  deleteMin,
  deleteFindMin,
  adjustMin,
  adjustMinA,
  adjustMinWithKey,
  adjustMinWithKeyA,
  updateMin,
  updateMinA,
  updateMinWithKey,
  updateMinWithKeyA,
  minView,
  minViewWithKey,
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

import qualified Data.List as List
import Data.Maybe (fromMaybe)

import Data.PQueue.Prio.Internals hiding (MinPQueue (..))
import Data.PQueue.Prio.Internals (MinPQueue)
import qualified Data.PQueue.Prio.Internals as Internals

import Prelude hiding (map, filter, break, span, takeWhile, dropWhile, splitAt, take, drop, (!!), null)

#ifdef __GLASGOW_HASKELL__
-- | A bidirectional pattern synonym for an empty priority queue.
--
-- @since 1.5.0
pattern Empty :: MinPQueue k a
pattern Empty = Internals.Empty
# if __GLASGOW_HASKELL__ >= 902
{-# INLINE CONLIKE Empty #-}
# endif

infixr 5 :<

-- | A bidirectional pattern synonym for working with the minimum view of a
-- 'MinPQueue'. Using @:<@ to construct a queue performs an insertion in
-- \(O(1)\) amortized time. When matching on @(k, a) :< q@, forcing @q@ takes
-- \(O(\log n)\) time.
--
-- @since 1.5.0
# if __GLASGOW_HASKELL__ >= 800
pattern (:<) :: Ord k => (k, a) -> MinPQueue k a -> MinPQueue k a
# else
pattern (:<) :: () => Ord k => (k, a) -> MinPQueue k a -> MinPQueue k a
# endif
pattern ka :< q <- (minViewWithKey -> Just (ka, q))
  where
    (k, a) :< q = insert k a q
# if __GLASGOW_HASKELL__ >= 902
{-# INLINE (:<) #-}
# endif

{-# COMPLETE Empty, (:<) #-}
#endif

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

infixr 8 .:

-- | \(O(1)\). The minimal (key, element) in the queue. Calls 'error' if empty.
findMin :: MinPQueue k a -> (k, a)
findMin = fromMaybe (error "Error: findMin called on an empty queue") . getMin

-- | \(O(\log n)\). Deletes the minimal (key, element) in the queue. Returns an empty queue
-- if the queue is empty.
deleteMin :: Ord k => MinPQueue k a -> MinPQueue k a
deleteMin = updateMin (const Nothing)

-- | \(O(\log n)\). Delete and find the element with the minimum key. Calls 'error' if empty.
deleteFindMin :: Ord k => MinPQueue k a -> ((k, a), MinPQueue k a)
deleteFindMin = fromMaybe (error "Error: deleteFindMin called on an empty queue") . minViewWithKey

-- | \(O(1)\). Alter the value at the minimum key. If the queue is empty, does nothing.
adjustMin :: (a -> a) -> MinPQueue k a -> MinPQueue k a
adjustMin = adjustMinWithKey . const

-- | \(O(1)\). Alter the value at the minimum key in an 'Applicative' context. If
-- the queue is empty, does nothing.
--
-- @since 1.4.2
adjustMinA :: Applicative f => (a -> f a) -> MinPQueue k a -> f (MinPQueue k a)
adjustMinA = adjustMinWithKeyA . const

-- | \(O(1)\) per operation. Alter the value at the minimum key in an 'Applicative' context. If the
-- queue is empty, does nothing.
--
-- @since 1.4.2
adjustMinWithKeyA :: Applicative f => (k -> a -> f a) -> MinPQueue k a -> f (MinPQueue k a)
adjustMinWithKeyA = adjustMinWithKeyA' id

-- | \(O(\log n)\). (Actually \(O(1)\) if there's no deletion.) Update the value at the minimum key.
-- If the queue is empty, does nothing.
updateMin :: Ord k => (a -> Maybe a) -> MinPQueue k a -> MinPQueue k a
updateMin = updateMinWithKey . const

-- | \(O(\log n)\) per operation. (Actually \(O(1)\) if there's no deletion.) Update
-- the value at the minimum key.  If the queue is empty, does nothing.
--
-- @since 1.4.2
updateMinA :: (Applicative f, Ord k) => (a -> f (Maybe a)) -> MinPQueue k a -> f (MinPQueue k a)
updateMinA = updateMinWithKeyA . const

-- | \(O(\log n)\) per operation. (Actually \(O(1)\) if there's no deletion.) Update
-- the value at the minimum key in an 'Applicative' context. If the queue is
-- empty, does nothing.
--
-- @since 1.4.2
updateMinWithKeyA :: (Applicative f, Ord k) => (k -> a -> f (Maybe a)) -> MinPQueue k a -> f (MinPQueue k a)
updateMinWithKeyA = updateMinWithKeyA' id

-- | \(O(\log n)\). Retrieves the value associated with the minimal key of the queue, and the queue
-- stripped of that element, or 'Nothing' if passed an empty queue.
minView :: Ord k => MinPQueue k a -> Maybe (a, MinPQueue k a)
minView q = do  ((_, a), q') <- minViewWithKey q
                return (a, q')

-- | \(O(n)\). Map a function over all values in the queue.
map :: (a -> b) -> MinPQueue k a -> MinPQueue k b
map = mapWithKey . const

-- | \(O(n)\). @'mapKeys' f q@ is the queue obtained by applying @f@ to each key of @q@.
mapKeys :: Ord k' => (k -> k') -> MinPQueue k a -> MinPQueue k' a
mapKeys f q = fromList [(f k, a) | (k, a) <- toListU q]

-- | \(O(n)\). Map values and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> MinPQueue k a -> MinPQueue k b
mapMaybe = mapMaybeWithKey . const

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> MinPQueue k a -> (MinPQueue k b, MinPQueue k c)
mapEither = mapEitherWithKey . const

-- | \(O(n)\). Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> MinPQueue k a -> MinPQueue k a
filter = filterWithKey . const

-- | \(O(n)\). Filter all values that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> MinPQueue k a
filterWithKey p = mapMaybeWithKey (\k a -> if p k a then Just a else Nothing)

-- | \(O(n)\). Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partition :: Ord k => (a -> Bool) -> MinPQueue k a -> (MinPQueue k a, MinPQueue k a)
partition = partitionWithKey . const

-- | \(O(n)\). Partition the queue according to a predicate. The first queue contains all elements
-- which satisfy the predicate, the second all elements that fail the predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> (MinPQueue k a, MinPQueue k a)
partitionWithKey p = mapEitherWithKey (\k a -> if p k a then Left a else Right a)

{-# INLINE take #-}
-- | \(O(k \log n)\). Takes the first @k@ (key, value) pairs in the queue, or the first @n@ if @k >= n@.
-- (@'take' k q == 'List.take' k ('toAscList' q)@)
take :: Ord k => Int -> MinPQueue k a -> [(k, a)]
take n = List.take n . toAscList

-- | \(O(k \log n)\). Deletes the first @k@ (key, value) pairs in the queue, or returns an empty queue if @k >= n@.
drop :: Ord k => Int -> MinPQueue k a -> MinPQueue k a
drop n0 q0
  | n0 <= 0  = q0
  | n0 >= size q0  = empty
  | otherwise  = drop' n0 q0
  where
    drop' n q
      | n == 0    = q
      | otherwise = drop' (n - 1) (deleteMin q)

-- | \(O(k \log n)\). Equivalent to @('take' k q, 'drop' k q)@.
splitAt :: Ord k => Int -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
splitAt n q
  | n <= 0     = ([], q)
  | otherwise  = n `seq` case minViewWithKey q of
      Just (ka, q') -> let (kas, q'') = splitAt (n - 1) q' in (ka : kas, q'')
      _             -> ([], q)

{-# INLINE takeWhile #-}
-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (p . 'snd') ('toAscList' q)@)
takeWhile :: Ord k => (a -> Bool) -> MinPQueue k a -> [(k, a)]
takeWhile = takeWhileWithKey . const

{-# INLINE takeWhileWithKey #-}
-- | Takes the longest possible prefix of elements satisfying the predicate.
-- (@'takeWhile' p q == 'List.takeWhile' (uncurry p) ('toAscList' q)@)
takeWhileWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> [(k, a)]
takeWhileWithKey p0 = List.takeWhile (uncurry' p0) . toAscList

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhile :: Ord k => (a -> Bool) -> MinPQueue k a -> MinPQueue k a
dropWhile = dropWhileWithKey . const

-- | Removes the longest possible prefix of elements satisfying the predicate.
dropWhileWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> MinPQueue k a
dropWhileWithKey p q = case minViewWithKey q of
  Just ((k, a), q')
    | p k a -> dropWhileWithKey p q'
  _         -> q

-- | Equivalent to @('takeWhile' p q, 'dropWhile' p q)@.
span :: Ord k => (a -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
span = spanWithKey . const

-- | Equivalent to @'span' ('not' . p)@.
break :: Ord k => (a -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
break p = span (not . p)

-- | Equivalent to @('takeWhileWithKey' p q, 'dropWhileWithKey' p q)@.
spanWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
spanWithKey p q = case minViewWithKey q of
  Just (t@(k, a), q')
    | p k a -> let (kas, q'') = spanWithKey p q' in (t : kas, q'')
  _         -> ([], q)

-- | Equivalent to @'spanWithKey' (\ k a -> 'not' (p k a)) q@.
breakWithKey :: Ord k => (k -> a -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
breakWithKey p = spanWithKey (not .: p)

-- | \(O(n)\). Build a priority queue from a descending list of (key, value) pairs. /The precondition is not checked./
fromDescList :: [(k, a)] -> MinPQueue k a
{-# INLINE fromDescList #-}
fromDescList xs = List.foldl' (\q (k, a) -> insertMin' k a q) empty xs

{-# INLINE keys #-}
-- | \(O(n \log n)\). Return all keys of the queue in ascending order.
keys :: Ord k => MinPQueue k a -> [k]
keys = List.map fst . toAscList

{-# INLINE elems #-}
-- | \(O(n \log n)\). Return all elements of the queue in ascending order by key.
elems :: Ord k => MinPQueue k a -> [a]
elems = List.map snd . toAscList

{-# INLINE toList #-}
-- | \(O(n \log n)\). Equivalent to 'toAscList'.
--
-- If the traversal order is irrelevant, consider using 'toListU'.
toList :: Ord k => MinPQueue k a -> [(k, a)]
toList = toAscList

{-# INLINE assocs #-}
-- | \(O(n \log n)\). Equivalent to 'toAscList'.
assocs :: Ord k => MinPQueue k a -> [(k, a)]
assocs = toAscList

{-# INLINE keysU #-}
-- | \(O(n)\). Return all keys of the queue in no particular order.
keysU :: MinPQueue k a -> [k]
keysU = List.map fst . toListU

{-# INLINE elemsU #-}
-- | \(O(n)\). Return all elements of the queue in no particular order.
elemsU :: MinPQueue k a -> [a]
elemsU = List.map snd . toListU

{-# INLINE assocsU #-}
-- | \(O(n)\). Equivalent to 'toListU'.
assocsU :: MinPQueue k a -> [(k, a)]
assocsU = toListU

-- | \(O(n)\). An unordered left fold over the elements of the queue, in no
-- particular order. This is rarely what you want; 'foldrU' and 'foldlU'' are
-- more likely to perform well.
foldlU :: (b -> a -> b) -> b -> MinPQueue k a -> b
foldlU f = foldlWithKeyU (const . f)

-- | \(O(n)\). An unordered strict left fold over the elements of the queue, in no
-- particular order.
--
-- @since 1.4.2
foldlU' :: (b -> a -> b) -> b -> MinPQueue k a -> b
foldlU' f = foldlWithKeyU' (const . f)

-- | \(O(n)\). An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseU :: (Applicative f) => (a -> f b) -> MinPQueue k a -> f (MinPQueue k b)
traverseU = traverseWithKeyU . const
