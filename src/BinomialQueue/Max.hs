{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  BinomialQueue.Max
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue. Unlike the queues in "Data.PQueue.Max",
-- these are /not/ augmented with a global root or their size, so 'getMax'
-- and 'size' take logarithmic, rather than constant, time. When those
-- operations are not (often) needed, these queues are generally faster than
-- those in "Data.PQueue.Max".
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
module BinomialQueue.Max (
  MaxQueue,
  -- * Basic operations
  empty,
  null,
  size,
  -- * Query operations
  findMax,
  getMax,
  deleteMax,
  deleteFindMax,
  maxView,
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

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif

import qualified Data.List as List

import qualified BinomialQueue.Min as MinQ
import Data.PQueue.Internals.Down

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

newtype MaxQueue a = MaxQueue { unMaxQueue :: MinQ.MinQueue (Down a) }

-- | /O(log n)/. Returns the minimum element. Throws an error on an empty queue.
findMax :: Ord a => MaxQueue a -> a
findMax = fromMaybe (error "Error: findMax called on empty queue") . getMax

-- | /O(1)/. The top (maximum) element of the queue, if there is one.
getMax :: Ord a => MaxQueue a -> Maybe a
getMax (MaxQueue q) = unDown <$> MinQ.getMin q

-- | /O(log n)/. Deletes the maximum element. If the queue is empty, does nothing.
deleteMax :: Ord a => MaxQueue a -> MaxQueue a
deleteMax = MaxQueue . MinQ.deleteMin . unMaxQueue

-- | /O(log n)/. Extracts the minimum element. Throws an error on an empty queue.
deleteFindMax :: Ord a => MaxQueue a -> (a, MaxQueue a)
deleteFindMax = fromMaybe (error "Error: deleteFindMax called on empty queue") . maxView

-- | /O(log n)/. Extract the top (maximum) element of the sequence, if there is one.
maxView :: Ord a => MaxQueue a -> Maybe (a, MaxQueue a)
maxView (MaxQueue q) = case MinQ.minView q of
  Just (Down a, q') -> Just (a, MaxQueue q')
  Nothing -> Nothing

-- | /O(k log n)/. Index (subscript) operator, starting from 0. @queue !! k@ returns the @(k+1)@th largest
-- element in the queue. Equivalent to @toDescList queue !! k@.
(!!) :: Ord a => MaxQueue a -> Int -> a
q !! n  | n >= size q
    = error "BinomialQueue.Max.!!: index too large"
q !! n = (List.!!) (toDescList q) n

{-# INLINE takeWhile #-}
-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: Ord a => (a -> Bool) -> MaxQueue a -> [a]
takeWhile p = fmap unDown . MinQ.takeWhile (p . unDown) . unMaxQueue

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
dropWhile p = MaxQueue . MinQ.dropWhile (p . unDown) . unMaxQueue

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
span :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
span p (MaxQueue queue)
  | (front, rear) <- MinQ.span (p . unDown) queue
  = (fmap unDown front, MaxQueue rear)

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
break p = span (not . p)

{-# INLINE take #-}
-- | /O(k log n)/. 'take' @k@, applied to a queue @queue@, returns a list of the greatest @k@ elements of @queue@,
-- or all elements of @queue@ itself if @k >= 'size' queue@.
take :: Ord a => Int -> MaxQueue a -> [a]
take n = List.take n . toDescList

-- | /O(k log n)/. 'drop' @k@, applied to a queue @queue@, returns @queue@ with the greatest @k@ elements deleted,
-- or an empty queue if @k >= size 'queue'@.
drop :: Ord a => Int -> MaxQueue a -> MaxQueue a
drop n (MaxQueue queue) = MaxQueue (MinQ.drop n queue)

-- | /O(k log n)/. Equivalent to @('take' k queue, 'drop' k queue)@.
splitAt :: Ord a => Int -> MaxQueue a -> ([a], MaxQueue a)
splitAt n (MaxQueue queue)
  | (l, r) <- MinQ.splitAt n queue
  = (fmap unDown l, MaxQueue r)

-- | /O(n)/. Returns the queue with all elements not satisfying @p@ removed.
filter :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
filter p = MaxQueue . MinQ.filter (p . unDown) . unMaxQueue

-- | /O(n)/. Returns a pair where the first queue contains all elements satisfying @p@, and the second queue
-- contains all elements not satisfying @p@.
partition :: Ord a => (a -> Bool) -> MaxQueue a -> (MaxQueue a, MaxQueue a)
partition p = go . unMaxQueue
  where
    go queue
      | (l, r) <- MinQ.partition (p . unDown) queue
      = (MaxQueue l, MaxQueue r)

-- | /O(n)/. Creates a new priority queue containing the images of the elements of this queue.
-- Equivalent to @'fromList' . 'Data.List.map' f . toList@.
map :: Ord b => (a -> b) -> MaxQueue a -> MaxQueue b
map f = MaxQueue . MinQ.map (fmap f) . unMaxQueue

{-# INLINE toList #-}
-- | /O(n log n)/. Returns the elements of the priority queue in descending order. Equivalent to 'toDescList'.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: Ord a => MaxQueue a -> [a]
toList = fmap unDown . MinQ.toAscList . unMaxQueue

toAscList :: Ord a => MaxQueue a -> [a]
toAscList = fmap unDown . MinQ.toDescList . unMaxQueue

toDescList :: Ord a => MaxQueue a -> [a]
toDescList = fmap unDown . MinQ.toAscList . unMaxQueue

-- | /O(n log n)/. Performs a right fold on the elements of a priority queue in descending order.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrDesc f z (MaxQueue q) = MinQ.foldrAsc (flip (foldr f)) z q

-- | /O(n log n)/. Performs a right fold on the elements of a priority queue in ascending order.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrAsc f z (MaxQueue q) = MinQ.foldrDesc (flip (foldr f)) z q

-- | /O(n log n)/. Performs a left fold on the elements of a priority queue in ascending order.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlAsc f z (MaxQueue q) = MinQ.foldlDesc (foldl f) z q

-- | /O(n log n)/. Performs a left fold on the elements of a priority queue in descending order.
foldlDesc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlDesc f z (MaxQueue q) = MinQ.foldlAsc (foldl f) z q

{-# INLINE fromAscList #-}
-- | /O(n)/. Constructs a priority queue from an ascending list. /Warning/: Does not check the precondition.
fromAscList :: [a] -> MaxQueue a
fromAscList = MaxQueue . MinQ.fromDescList . fmap Down

{-# INLINE fromDescList #-}
-- | /O(n)/. Constructs a priority queue from a descending list. /Warning/: Does not check the precondition.
fromDescList :: [a] -> MaxQueue a
fromDescList = MaxQueue . MinQ.fromAscList . fmap Down

fromList :: Ord a => [a] -> MaxQueue a
fromList = MaxQueue . MinQ.fromList . fmap Down

-- | Equivalent to 'toListU'.
elemsU :: MaxQueue a -> [a]
elemsU = toListU

-- | Convert to a list in an arbitrary order.
toListU :: MaxQueue a -> [a]
toListU = fmap unDown . MinQ.toListU . unMaxQueue

-- | Get the number of elements in a 'MaxQueue'.
size :: MaxQueue a -> Int
size = MinQ.size . unMaxQueue

empty :: MaxQueue a
empty = MaxQueue MinQ.empty

foldMapU :: Monoid m => (a -> m) -> MaxQueue a -> m
foldMapU f = MinQ.foldMapU (f . unDown) . unMaxQueue

seqSpine :: MaxQueue a -> b -> b
seqSpine = MinQ.seqSpine . unMaxQueue

foldlU :: (b -> a -> b) -> b -> MaxQueue a -> b
foldlU f b = MinQ.foldlU (\acc (Down a) -> f acc a) b . unMaxQueue

foldlU' :: (b -> a -> b) -> b -> MaxQueue a -> b
foldlU' f b = MinQ.foldlU' (\acc (Down a) -> f acc a) b . unMaxQueue

foldrU :: (a -> b -> b) -> b -> MaxQueue a -> b
foldrU c n = MinQ.foldrU (c . unDown) n . unMaxQueue

null :: MaxQueue a -> Bool
null = MinQ.null . unMaxQueue

singleton :: a -> MaxQueue a
singleton = MaxQueue . MinQ.singleton . Down

mapMaybe :: Ord b => (a -> Maybe b) -> MaxQueue a -> MaxQueue b
mapMaybe f = MaxQueue . MinQ.mapMaybe (fmap Down . f . unDown) . unMaxQueue

insert :: Ord a => a -> MaxQueue a -> MaxQueue a
insert a (MaxQueue q) = MaxQueue (MinQ.insert (Down a) q)

mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MaxQueue a -> (MaxQueue b, MaxQueue c)
mapEither f (MaxQueue q) = case MinQ.mapEither (bimap Down Down . f . unDown) q of
  (l, r) -> (MaxQueue l, MaxQueue r)

union :: Ord a => MaxQueue a -> MaxQueue a -> MaxQueue a
union (MaxQueue a) (MaxQueue b) = MaxQueue (MinQ.union a b)

unions :: Ord a => [MaxQueue a] -> MaxQueue a
unions = MaxQueue . MinQ.unions . fmap unMaxQueue
