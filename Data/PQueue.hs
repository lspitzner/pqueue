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
-- General purpose priority queue, supporting view-first operations.
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
  findFirst,
  getFirst,
  deleteFirst,
  deleteFindFirst,
  delete,
  firstView,
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
import Data.PQueue.Prio.Private (PQueue(PQ))
import Data.PQueue.Top (First, TaggedF(TaggedF, unTaggedF), switch)

import Control.DeepSeq (NFData(rnf))

import Data.Functor ((<$>))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
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

-- | A priority queue with elements of type @a@.  Supports extracting the first element.
-- Implemented as a TaggedFper around 'Min.MinQueue'.
newtype Queue first a = Q (Min.MinQueue (TaggedF first a))
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data, Typeable)
# else
  deriving (Eq, Ord)
# endif

type MinQueue = Queue Top.Min
type MaxQueue = Queue Top.Max

instance NFData a => NFData (Queue first a) where
  rnf (Q q) = rnf q

instance (First first, Ord a, Show a) => Show (Queue first a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromOrderedList " . shows (toList xs)

instance Read a => Read (Queue first a) where
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

instance (First first, Ord a) => Monoid (Queue first a) where
  mempty = empty
  mappend = union

unSwitchQueue
  :: (Min.MinQueue (TaggedF first a) -> TaggedF first b) -> Queue first a -> b
unSwitchQueue f (Q q) = unTaggedF (f q)

unSwitchQueueConstr
  :: TaggedF first (Min.MinQueue (TaggedF first a)) -> Queue first a
unSwitchQueueConstr q = Q (unTaggedF q)

-- | /O(1)/.  The empty priority queue.
empty :: Queue first a
empty = Q Min.empty

-- | /O(1)/.  Is this the empty priority queue?
null :: Queue first a -> Bool
null (Q q) = Min.null q

-- | /O(1)/.  The number of elements in the queue.
size :: Queue first a -> Int
size (Q q) = Min.size q

-- | /O(1)/.  Returns the first element of the queue.  Throws an error on an empty queue.
findFirst :: Queue first a -> a
findFirst = fromMaybe (error "Error: findFirst called on empty queue") . getFirst

-- | /O(1)/.  The first element of the queue, if there is one.
getFirst :: Queue first a -> Maybe a
getFirst (Q q) = unTaggedF <$> Min.getMin q

-- | /O(log n)/.  Deletes the first element of the queue.  Does nothing on an empty queue.
deleteFirst :: (First first, Ord a) => Queue first a -> Queue first a
deleteFirst (Q q) = Q (Min.deleteMin q)

-- | /O(log n)/.  Extracts the first element of the queue.  Throws an error on an empty queue.
deleteFindFirst :: (First first, Ord a) => Queue first a -> (a, Queue first a)
deleteFindFirst = fromMaybe (error "Error: deleteFindFirst called on empty queue") . firstView

-- | /O(log n)/.  Extract the first element of the sequence, if there is one.
firstView :: (First first, Ord a) => Queue first a -> Maybe (a, Queue first a)
firstView (Q q) = case Min.minView q of
  Nothing              -> Nothing
  Just (TaggedF x, q') -> Just (x, Q q')

-- | /O(log n)/.  Delete the first element of the sequence, if there is one.
delete :: (First first, Ord a) => Queue first a -> Maybe (Queue first a)
delete = fmap snd . firstView

-- | /O(1)/.  Construct a priority queue with a single element.
singleton :: a -> Queue first a
singleton = Q . Min.singleton . TaggedF

-- | /O(1)/.  Insert an element into the priority queue.
insert :: (First first, Ord a) => a -> Queue first a -> Queue first a
x `insert` Q q = Q (TaggedF x `Min.insert` q)

-- | Amortized /O(1)/, worst-case /O(log n)/.  Insert an element into the priority queue,
--   putting it behind elements that compare equal to the inserted one.
insertBehind :: (First first, Ord a) => a -> Queue first a -> Queue first a
x `insertBehind` Q q = Q (TaggedF x `Min.insertBehind` q)

-- | /O(log (min(n1,n2)))/.  Take the union of two priority queues.
union :: (First first, Ord a) => Queue first a -> Queue first a -> Queue first a
Q q1 `union` Q q2 = Q (q1 `Min.union` q2)

-- | Takes the union of a list of priority queues.  Equivalent to @'foldl' 'union' 'empty'@.
unions :: (First first, Ord a) => [Queue first a] -> Queue first a
unions qs = Q (Min.unions [q | Q q <- qs])

-- | /O(k log n)/.  Returns the @(k+1)@th first element of the queue.
(!!) :: (First first, Ord a) => Queue first a -> Int -> a
Q q !! n = unTaggedF ((Min.!!) q n)

{-# INLINE take #-}
-- | /O(k log n)/.  Returns the list of the @k@ first elements of the queue, in natural order, or
-- all elements of the queue, if @k >= n@.
take :: (First first, Ord a) => Int -> Queue first a -> [a]
take k (Q q) = map unTaggedF $ Min.take k q

-- | /O(k log n)/.  Returns the queue with the @k@ first elements deleted, or the empty queue if @k >= n@.
drop :: (First first, Ord a) => Int -> Queue first a -> Queue first a
drop k (Q q) = Q (Min.drop k q)

-- | /O(k log n)/.  Equivalent to @(take k queue, drop k queue)@.
splitAt :: (First first, Ord a) => Int -> Queue first a -> ([a], Queue first a)
splitAt k (Q q) = (map unTaggedF xs, Q q') where
  (xs, q') = Min.splitAt k q

-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: (First first, Ord a) => (a -> Bool) -> Queue first a -> [a]
takeWhile p (Q q) = map unTaggedF (Min.takeWhile (p . unTaggedF) q)

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: (First first, Ord a) => (a -> Bool) -> Queue first a -> Queue first a
dropWhile p (Q q) = Q (Min.dropWhile (p . unTaggedF) q)

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
--
span :: (First first, Ord a) => (a -> Bool) -> Queue first a -> ([a], Queue first a)
span p (Q q) = (map unTaggedF xs, Q q') where
  (xs, q') = Min.span (p . unTaggedF) q

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: (First first, Ord a) => (a -> Bool) -> Queue first a -> ([a], Queue first a)
break p = span (not . p)

-- | /O(n)/.  Returns a queue of those elements which satisfy the predicate.
filter :: (First first, Ord a) => (a -> Bool) -> Queue first a -> Queue first a
filter p (Q q) = Q (Min.filter (p . unTaggedF) q)

-- | /O(n)/.  Returns a pair of queues, where the left queue contains those elements that satisfy the predicate,
-- and the right queue contains those that do not.
partition :: (First first, Ord a) => (a -> Bool) -> Queue first a -> (Queue first a, Queue first a)
partition p (Q q) = (Q q0, Q q1)
  where  (q0, q1) = Min.partition (p . unTaggedF) q

-- | /O(n)/.  Maps a function over the elements of the queue, and collects the 'Just' values.
mapMaybe :: (First first, Ord b) => (a -> Maybe b) -> Queue first a -> Queue first b
mapMaybe f (Q q) = Q (Min.mapMaybe (traverse f) q)

-- | /O(n)/.  Maps a function over the elements of the queue, and separates the 'Left' and 'Right' values.
mapEither :: (First first, Ord b, Ord c) => (a -> Either b c) -> Queue first a -> (Queue first b, Queue first c)
mapEither f (Q q) = (Q q0, Q q1)
  where  (q0, q1) = Min.mapEither (either (Left . TaggedF) (Right . TaggedF) . f . unTaggedF) q

-- | /O(n)/.  Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue.
-- /Does not check the precondition/.
mapU :: (a -> b) -> Queue first a -> Queue first b
mapU f (Q q) = Q (Min.mapU (fmap f) q)

-- | /O(n)/.  Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> Queue first a -> b
foldrU f z (Q q) = Min.foldrU (flip (foldr f)) z q

-- | /O(n)/.  Unordered left fold on a priority queue.
foldlU :: (b -> a -> b) -> b -> Queue first a -> b
foldlU f z (Q q) = Min.foldlU (foldl f) z q

{-# INLINE elemsU #-}
-- | Equivalent to 'toListU'.
elemsU :: Queue first a -> [a]
elemsU = toListU

{-# INLINE toListU #-}
-- | /O(n)/.  Returns a list of the elements of the priority queue, in no particular order.
toListU :: Queue first a -> [a]
toListU (Q q) = map unTaggedF (Min.toListU q)

-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in ascending order.
-- @'foldrAsc' f z q == 'foldlDesc' (flip f) z q@.
foldrAsc :: (First first, Ord a) => (a -> b -> b) -> b -> Queue first a -> b
foldrAsc = foldlDesc . flip

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in descending order.
-- @'foldlAsc' f z q == 'foldrDesc' (flip f) z q@.
foldlAsc :: (First first, Ord a) => (b -> a -> b) -> b -> Queue first a -> b
foldlAsc = foldrDesc . flip

-- newtype
--   Foldr b a first =
--     Foldr {
--       runFoldr :: (TaggedF first a -> b -> b) -> b -> Min.MinQueue (TaggedF first a) -> b
--     }

-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in descending order.
foldrDesc :: (First first, Ord a) => (a -> b -> b) -> b -> Queue first a -> b
foldrDesc f z = unSwitchQueue $ \q ->
  switch (Min.foldrDesc (f . unTaggedF) z q) (Min.foldrAsc (f . unTaggedF) z q)
--   runFoldr
--     (Top.switch (Foldr Min.foldrDesc) (Foldr Min.foldrAsc))
--     (flip (foldr f)) z q

-- newtype
--   Foldl b a first =
--     Foldl {
--       runFoldl :: (b -> TaggedF first a -> b) -> b -> Min.MinQueue (TaggedF first a) -> b
--     }

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in descending order.
foldlDesc :: (First first, Ord a) => (b -> a -> b) -> b -> Queue first a -> b
foldlDesc f z = unSwitchQueue $ \q ->
  switch (Min.foldlDesc (foldl f) z q) (Min.foldlAsc (foldl f) z q)
  -- runFoldl
  --   (Top.switch (Foldl Min.foldlDesc) (Foldl Min.foldlAsc))
  --   (foldl f) z q

-- newtype
--   ToList a first =
--     ToList {runToList :: Min.MinQueue (TaggedF first a) -> [TaggedF first a]}

{-# INLINE toAscList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in ascending order.
toAscList :: (First first, Ord a) => Queue first a -> [a]
toAscList = map unTaggedF
  . unSwitchQueue (\q -> switch (Min.toAscList q) (Min.toDescList q))
  -- map unTaggedF $
  -- runToList (Top.switch (ToList Min.toAscList) (ToList Min.toDescList)) q

{-# INLINE toDescList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in descending order.
toDescList :: (First first, Ord a) => Queue first a -> [a]
toDescList = map unTaggedF
  . unSwitchQueue (\q -> switch (Min.toDescList q) (Min.toAscList q))
  -- runToList (Top.switch (ToList Min.toDescList) (ToList Min.toAscList)) q

{-# INLINE toList #-}
-- | /O(n log n)/.  Returns the elements of the priority queue with first keys first.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: (First first, Ord a) => Queue first a -> [a]
toList (Q q) = map unTaggedF (Min.toList q)

-- newtype
--   FromList a first =
--     FromList {runFromList :: [TaggedF first a] -> Min.MinQueue (TaggedF first a)}

{-# INLINE fromAscList #-}
-- | /O(n)/.  Constructs a priority queue from an ascending list.  /Warning/: Does not check the precondition.
fromAscList :: (First first) => [a] -> Queue first a
fromAscList xs =
  unSwitchQueueConstr $ switch (Min.fromAscList txs) (Min.fromDescList txs)
  where
    txs = map TaggedF xs
  -- Q
  --   . runFromList
  --       (switch (FromList Min.fromAscList) (FromList Min.fromDescList))
  --   . map TaggedF

{-# INLINE fromDescList #-}
-- | /O(n)/.  Constructs a priority queue from a descending list.  /Warning/: Does not check the precondition.
fromDescList :: (First first) => [a] -> Queue first a
fromDescList xs =
  unSwitchQueueConstr $ switch (Min.fromDescList txs) (Min.fromAscList txs)
  where
    txs = map TaggedF xs
  -- Q
  --   . runFromList
  --       (Top.switch (FromList Min.fromDescList) (FromList Min.fromAscList))
  --   . map TaggedF

{-# INLINE fromOrderedList #-}
-- | /O(n)/.  Constructs a priority queue from a list with first keys first.  /Warning/: Does not check the precondition.
fromOrderedList :: [a] -> Queue first a
fromOrderedList = Q . Min.fromAscList . map TaggedF

{-# INLINE fromList #-}
-- | /O(n log n)/.  Constructs a priority queue from an unordered list.
fromList :: (First first, Ord a) => [a] -> Queue first a
fromList = foldr insert empty

-- | /O(n)/.  Constructs a priority queue from the keys of a 'Prio.PQueue'.
keysQueue :: PQueue first k a -> Queue first k
keysQueue (PQ q) = Q (Min.keysQueue q)

-- | /O(log n)/.  Forces the spine of the heap.
seqSpine :: Queue first a -> b -> b
seqSpine (Q q) = Min.seqSpine q
