{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue
-- Copyright   :  (c) Henning Thielemann 2017
--                (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue, supporting view-top operations.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /k/ being the integral index used by
-- some operations.  These bounds hold even in a persistent (shared) setting.
--
-- This implementation is based on a binomial heap augmented with a global root.
-- The spine of the heap is maintained lazily.  To force the spine of the heap,
-- use 'seqSpine'.
--
-- This implementation does not guarantee stable behavior.
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered.  No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue (
  Queue,
  MinQueue,
  MaxQueue,
  -- * Basic operations
  empty,
  null,
  size,
  -- * Query operations
  findTop,
  getTop,
  deleteTop,
  deleteFindTop,
  delete,
  topView,
  -- * Construction operations
  singleton,
  insert,
  insertBehind,
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
  fromOrderedList,
  -- * Unordered operations
  mapU,
  foldrU,
  foldlU,
  elemsU,
  toListU,
  -- * Miscellaneous operations
  keysQueue,
  seqSpine) where

import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Top as Top
import qualified Data.PQueue.Prio as Prio
import Data.PQueue.Top (Top, Wrap(Wrap, unwrap))

import Control.DeepSeq (NFData(rnf))

import Data.Functor ((<$>))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl, foldr)

import Prelude hiding (null, foldr, foldl, take, drop, takeWhile, dropWhile, splitAt, span, break, (!!), filter)

#ifdef __GLASGOW_HASKELL__
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
import Data.Data
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

-- | A priority queue with elements of type @a@.  Supports extracting the top element.
-- Implemented as a wrapper around 'Min.MinQueue'.
newtype Queue top a = Q (Min.MinQueue (Wrap top a))
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data, Typeable)
# else
  deriving (Eq, Ord)
# endif

type MinQueue = Queue Top.Min
type MaxQueue = Queue Top.Max

instance NFData a => NFData (Queue top a) where
  rnf (Q q) = rnf q

instance (Top top, Ord a, Show a) => Show (Queue top a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromOrderedList " . shows (toList xs)

instance Read a => Read (Queue top a) where
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

instance (Top top, Ord a) => Monoid (Queue top a) where
  mempty = empty
  mappend = union

-- | /O(1)/.  The empty priority queue.
empty :: Queue top a
empty = Q Min.empty

-- | /O(1)/.  Is this the empty priority queue?
null :: Queue top a -> Bool
null (Q q) = Min.null q

-- | /O(1)/.  The number of elements in the queue.
size :: Queue top a -> Int
size (Q q) = Min.size q

-- | /O(1)/.  Returns the top element of the queue.  Throws an error on an empty queue.
findTop :: Queue top a -> a
findTop = fromMaybe (error "Error: findTop called on empty queue") . getTop

-- | /O(1)/.  The top element of the queue, if there is one.
getTop :: Queue top a -> Maybe a
getTop (Q q) = unwrap <$> Min.getMin q

-- | /O(log n)/.  Deletes the top element of the queue.  Does nothing on an empty queue.
deleteTop :: (Top top, Ord a) => Queue top a -> Queue top a
deleteTop (Q q) = Q (Min.deleteMin q)

-- | /O(log n)/.  Extracts the top element of the queue.  Throws an error on an empty queue.
deleteFindTop :: (Top top, Ord a) => Queue top a -> (a, Queue top a)
deleteFindTop = fromMaybe (error "Error: deleteFindTop called on empty queue") . topView

-- | /O(log n)/.  Extract the top element of the sequence, if there is one.
topView :: (Top top, Ord a) => Queue top a -> Maybe (a, Queue top a)
topView (Q q) = case Min.minView q of
  Nothing -> Nothing
  Just (Wrap x, q')
          -> Just (x, Q q')

-- | /O(log n)/.  Delete the top element of the sequence, if there is one.
delete :: (Top top, Ord a) => Queue top a -> Maybe (Queue top a)
delete = fmap snd . topView

-- | /O(1)/.  Construct a priority queue with a single element.
singleton :: a -> Queue top a
singleton = Q . Min.singleton . Wrap

-- | /O(1)/.  Insert an element into the priority queue.
insert :: (Top top, Ord a) => a -> Queue top a -> Queue top a
x `insert` Q q = Q (Wrap x `Min.insert` q)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Insert an element into the priority queue,
--   putting it behind elements that compare equal to the inserted one.
insertBehind :: (Top top, Ord a) => a -> Queue top a -> Queue top a
x `insertBehind` Q q = Q (Wrap x `Min.insertBehind` q)

-- | /O(log (min(n1,n2)))/.  Take the union of two priority queues.
union :: (Top top, Ord a) => Queue top a -> Queue top a -> Queue top a
Q q1 `union` Q q2 = Q (q1 `Min.union` q2)

-- | Takes the union of a list of priority queues.  Equivalent to @'foldl' 'union' 'empty'@.
unions :: (Top top, Ord a) => [Queue top a] -> Queue top a
unions qs = Q (Min.unions [q | Q q <- qs])

-- | /O(k log n)/.  Returns the @(k+1)@th top element of the queue.
(!!) :: (Top top, Ord a) => Queue top a -> Int -> a
Q q !! n = unwrap ((Min.!!) q n)

{-# INLINE take #-}
-- | /O(k log n)/.  Returns the list of the @k@ top elements of the queue, in natural order, or
-- all elements of the queue, if @k >= n@.
take :: (Top top, Ord a) => Int -> Queue top a -> [a]
take k (Q q) = map unwrap $ Min.take k q

-- | /O(k log n)/.  Returns the queue with the @k@ top elements deleted, or the empty queue if @k >= n@.
drop :: (Top top, Ord a) => Int -> Queue top a -> Queue top a
drop k (Q q) = Q (Min.drop k q)

-- | /O(k log n)/.  Equivalent to @(take k queue, drop k queue)@.
splitAt :: (Top top, Ord a) => Int -> Queue top a -> ([a], Queue top a)
splitAt k (Q q) = (map unwrap xs, Q q') where
  (xs, q') = Min.splitAt k q

-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> [a]
takeWhile p (Q q) = map unwrap (Min.takeWhile (p . unwrap) q)

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> Queue top a
dropWhile p (Q q) = Q (Min.dropWhile (p . unwrap) q)

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
--
span :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> ([a], Queue top a)
span p (Q q) = (map unwrap xs, Q q') where
  (xs, q') = Min.span (p . unwrap) q

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> ([a], Queue top a)
break p = span (not . p)

-- | /O(n)/.  Returns a queue of those elements which satisfy the predicate.
filter :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> Queue top a
filter p (Q q) = Q (Min.filter (p . unwrap) q)

-- | /O(n)/.  Returns a pair of queues, where the left queue contains those elements that satisfy the predicate,
-- and the right queue contains those that do not.
partition :: (Top top, Ord a) => (a -> Bool) -> Queue top a -> (Queue top a, Queue top a)
partition p (Q q) = (Q q0, Q q1)
  where  (q0, q1) = Min.partition (p . unwrap) q

-- | /O(n)/.  Maps a function over the elements of the queue, and collects the 'Just' values.
mapMaybe :: (Top top, Ord b) => (a -> Maybe b) -> Queue top a -> Queue top b
mapMaybe f (Q q) = Q (Min.mapMaybe (\ (Wrap x) -> Wrap <$> f x) q)

-- | /O(n)/.  Maps a function over the elements of the queue, and separates the 'Left' and 'Right' values.
mapEither :: (Top top, Ord b, Ord c) => (a -> Either b c) -> Queue top a -> (Queue top b, Queue top c)
mapEither f (Q q) = (Q q0, Q q1)
  where  (q0, q1) = Min.mapEither (either (Left . Wrap) (Right . Wrap) . f . unwrap) q

-- | /O(n)/.  Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue.
-- /Does not check the precondition/.
mapU :: (a -> b) -> Queue top a -> Queue top b
mapU f (Q q) = Q (Min.mapU (fmap f) q)

-- | /O(n)/.  Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> Queue top a -> b
foldrU f z (Q q) = Min.foldrU (flip (foldr f)) z q

-- | /O(n)/.  Unordered left fold on a priority queue.
foldlU :: (b -> a -> b) -> b -> Queue top a -> b
foldlU f z (Q q) = Min.foldlU (foldl f) z q

{-# INLINE elemsU #-}
-- | Equivalent to 'toListU'.
elemsU :: Queue top a -> [a]
elemsU = toListU

{-# INLINE toListU #-}
-- | /O(n)/.  Returns a list of the elements of the priority queue, in no particular order.
toListU :: Queue top a -> [a]
toListU (Q q) = map unwrap (Min.toListU q)

-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in ascending order.
-- @'foldrAsc' f z q == 'foldlDesc' (flip f) z q@.
foldrAsc :: (Top top, Ord a) => (a -> b -> b) -> b -> Queue top a -> b
foldrAsc = foldlDesc . flip

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in descending order.
-- @'foldlAsc' f z q == 'foldrDesc' (flip f) z q@.
foldlAsc :: (Top top, Ord a) => (b -> a -> b) -> b -> Queue top a -> b
foldlAsc = foldrDesc . flip

newtype
  Foldr b a top =
    Foldr {
      runFoldr :: (Wrap top a -> b -> b) -> b -> Min.MinQueue (Wrap top a) -> b
    }

-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in descending order.
foldrDesc :: (Top top, Ord a) => (a -> b -> b) -> b -> Queue top a -> b
foldrDesc f z (Q q) =
  runFoldr
    (Top.switch (Foldr Min.foldrDesc) (Foldr Min.foldrAsc))
    (flip (foldr f)) z q

newtype
  Foldl b a top =
    Foldl {
      runFoldl :: (b -> Wrap top a -> b) -> b -> Min.MinQueue (Wrap top a) -> b
    }

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in descending order.
foldlDesc :: (Top top, Ord a) => (b -> a -> b) -> b -> Queue top a -> b
foldlDesc f z (Q q) =
  runFoldl
    (Top.switch (Foldl Min.foldlDesc) (Foldl Min.foldlAsc))
    (foldl f) z q

newtype
  ToList a top =
    ToList {runToList :: Min.MinQueue (Wrap top a) -> [Wrap top a]}

{-# INLINE toAscList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in ascending order.
toAscList :: (Top top, Ord a) => Queue top a -> [a]
toAscList (Q q) =
  fmap unwrap $
  runToList (Top.switch (ToList Min.toAscList) (ToList Min.toDescList)) q

{-# INLINE toDescList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in descending order.
toDescList :: (Top top, Ord a) => Queue top a -> [a]
toDescList (Q q) =
  fmap unwrap $
  runToList (Top.switch (ToList Min.toDescList) (ToList Min.toAscList)) q

{-# INLINE toList #-}
-- | /O(n log n)/.  Returns the elements of the priority queue with top keys first.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: (Top top, Ord a) => Queue top a -> [a]
toList (Q q) = map unwrap (Min.toList q)

newtype
  FromList a top =
    FromList {runFromList :: [Wrap top a] -> Min.MinQueue (Wrap top a)}

{-# INLINE fromAscList #-}
-- | /O(n)/.  Constructs a priority queue from an ascending list.  /Warning/: Does not check the precondition.
fromAscList :: (Top top) => [a] -> Queue top a
fromAscList =
  Q .
  runFromList
    (Top.switch (FromList Min.fromAscList) (FromList Min.fromDescList)) .
  map Wrap

{-# INLINE fromDescList #-}
-- | /O(n)/.  Constructs a priority queue from a descending list.  /Warning/: Does not check the precondition.
fromDescList :: (Top top) => [a] -> Queue top a
fromDescList =
  Q .
  runFromList
    (Top.switch (FromList Min.fromDescList) (FromList Min.fromAscList)) .
  map Wrap

{-# INLINE fromOrderedList #-}
-- | /O(n)/.  Constructs a priority queue from a list with top keys first.  /Warning/: Does not check the precondition.
fromOrderedList :: [a] -> Queue top a
fromOrderedList = Q . Min.fromAscList . map Wrap

{-# INLINE fromList #-}
-- | /O(n log n)/.  Constructs a priority queue from an unordered list.
fromList :: (Top top, Ord a) => [a] -> Queue top a
fromList = foldr insert empty

-- | /O(n)/.  Constructs a priority queue from the keys of a 'Prio.PQueue'.
keysQueue :: Prio.PQueue top k a -> Queue top k
keysQueue (Prio.PQ q) = Q (Min.keysQueue q)

-- | /O(log n)/.  Forces the spine of the heap.
seqSpine :: Queue top a -> b -> b
seqSpine (Q q) = Min.seqSpine q
