{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  BinomialQueue.Min
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue. Unlike the queues in "Data.PQueue.Min",
-- these are /not/ augmented with a global root or their size, so 'getMin'
-- and 'size' take logarithmic, rather than constant, time. When those
-- operations are not (often) needed, these queues are generally faster than
-- those in "Data.PQueue.Min".
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /k/ being the integral index used by
-- some operations. These bounds hold even in a persistent (shared) setting.
--
-- This implementation is based on a binomial heap.
--
-- This implementation does not guarantee stable behavior.
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered. No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module BinomialQueue.Min (
  MinQueue,
  -- * Basic operations
  empty,
  null,
  size,
  -- * Query operations
  findMin,
  getMin,
  deleteMin,
  deleteFindMin,
  minView,
  -- * Construction operations
  singleton,
  insert,
  union,
  unions,
  -- * Subsets
  -- ** Extracting subsets
  (!!),
  take,
  drop,
  splitAt,
  -- ** Predicates
  takeWhile,
  dropWhile,
  span,
  break,
  -- * Filter/Map
  filter,
  partition,
  mapMaybe,
  mapEither,
  -- * Fold\/Functor\/Traversable variations
  map,
  mapMonotonic,
  foldrAsc,
  foldlAsc,
  foldrDesc,
  foldlDesc,
  -- * List operations
  toList,
  toAscList,
  toDescList,
  fromList,
  fromAscList,
  fromDescList,
  -- * Unordered operations
  foldrU,
  foldlU,
  foldlU',
  foldMapU,
  elemsU,
  toListU,
  -- * Miscellaneous operations
--  keysQueue,  -- We want bare Prio queues for this.
  seqSpine
  ) where

import Prelude hiding (null, take, drop, takeWhile, dropWhile, splitAt, span, break, (!!), filter, map)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif
import qualified Data.List as List
import Data.Maybe (fromMaybe)

import BinomialQueue.Internals

-- | \(O(\log n)\). Returns the minimum element. Throws an error on an empty queue.
findMin :: Ord a => MinQueue a -> a
findMin = fromMaybe (error "Error: findMin called on empty queue") . getMin

-- | \(O(\log n)\). Deletes the minimum element. If the queue is empty, does nothing.
deleteMin :: Ord a => MinQueue a -> MinQueue a
deleteMin q = case minView q of
  Nothing      -> empty
  Just (_, q') -> q'

-- | \(O(\log n)\). Extracts the minimum element. Throws an error on an empty queue.
deleteFindMin :: Ord a => MinQueue a -> (a, MinQueue a)
deleteFindMin = fromMaybe (error "Error: deleteFindMin called on empty queue") . minView

-- | \(O(k \log n)\). Index (subscript) operator, starting from 0. @queue !! k@ returns the @(k+1)@th smallest
-- element in the queue. Equivalent to @toAscList queue !! k@.
(!!) :: Ord a => MinQueue a -> Int -> a
q !! n  | n >= size q
    = error "Data.PQueue.Min.!!: index too large"
q !! n = toAscList q List.!! n

{-# INLINE takeWhile #-}
-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: Ord a => (a -> Bool) -> MinQueue a -> [a]
takeWhile p = List.takeWhile p . toAscList

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: Ord a => (a -> Bool) -> MinQueue a -> MinQueue a
dropWhile p = drop' where
  drop' q = case minView q of
    Just (x, q') | p x -> drop' q'
    _                  -> q

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
span :: Ord a => (a -> Bool) -> MinQueue a -> ([a], MinQueue a)
span p queue = case minView queue of
  Just (x, q')
    | p x  -> let (ys, q'') = span p q' in (x : ys, q'')
  _        -> ([], queue)

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: Ord a => (a -> Bool) -> MinQueue a -> ([a], MinQueue a)
break p = span (not . p)

{-# INLINE take #-}
-- | \(O(k \log n)\). 'take' @k@, applied to a queue @queue@, returns a list of the smallest @k@ elements of @queue@,
-- or all elements of @queue@ itself if @k >= 'size' queue@.
take :: Ord a => Int -> MinQueue a -> [a]
take n = List.take n . toAscList

-- | \(O(k \log n)\). 'drop' @k@, applied to a queue @queue@, returns @queue@ with the smallest @k@ elements deleted,
-- or an empty queue if @k >= 'size' queue@.
drop :: Ord a => Int -> MinQueue a -> MinQueue a
drop n queue = n `seq` case minView queue of
  Just (_, queue')
    | n > 0  -> drop (n - 1) queue'
  _          -> queue

-- | \(O(k \log n)\). Equivalent to @('take' k queue, 'drop' k queue)@.
splitAt :: Ord a => Int -> MinQueue a -> ([a], MinQueue a)
splitAt n queue = n `seq` case minView queue of
  Just (x, queue')
    | n > 0  -> let (xs, queue'') = splitAt (n - 1) queue' in (x : xs, queue'')
  _          -> ([], queue)

-- | \(O(n)\). Returns the queue with all elements not satisfying @p@ removed.
filter :: Ord a => (a -> Bool) -> MinQueue a -> MinQueue a
filter p = mapMaybe (\x -> if p x then Just x else Nothing)

-- | \(O(n)\). Returns a pair where the first queue contains all elements satisfying @p@, and the second queue
-- contains all elements not satisfying @p@.
partition :: Ord a => (a -> Bool) -> MinQueue a -> (MinQueue a, MinQueue a)
partition p = mapEither (\x -> if p x then Left x else Right x)

-- | \(O(n)\). Creates a new priority queue containing the images of the elements of this queue.
-- Equivalent to @'fromList' . 'Data.List.map' f . toList@.
map :: Ord b => (a -> b) -> MinQueue a -> MinQueue b
map f = foldrU (insert . f) empty

{-# INLINE toList #-}
-- | \(O(n \log n)\). Returns the elements of the priority queue in ascending order. Equivalent to 'toAscList'.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: Ord a => MinQueue a -> [a]
toList = toAscList

-- | \(O(n \log n)\). Performs a left fold on the elements of a priority queue in descending order.
-- @foldlDesc f z q == foldrAsc (flip f) z q@.
foldlDesc :: Ord a => (b -> a -> b) -> b -> MinQueue a -> b
foldlDesc = foldrAsc . flip

{-# INLINE fromDescList #-}
-- | \(O(n)\). Constructs a priority queue from an descending list. /Warning/: Does not check the precondition.
fromDescList :: [a] -> MinQueue a
-- We apply an explicit argument to get foldl' to inline.
fromDescList xs = foldl' (flip insertMinQ') empty xs

-- | Equivalent to 'toListU'.
elemsU :: MinQueue a -> [a]
elemsU = toListU
