{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Max
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue, supporting view-maximum operations.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /k/ being the integral index used by
-- some operations. These bounds hold even in a persistent (shared) setting.
--
-- This implementation is based on a binomial heap augmented with a global root.
--
-- This implementation does not guarantee stable behavior.
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered. No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue.Max (
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
  delete,
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
  mapU,
  foldrU,
  foldlU,
  foldlU',
  foldMapU,
  elemsU,
  toListU,
  -- * Miscellaneous operations
  keysQueue,
  seqSpine) where

import Control.DeepSeq (NFData(rnf))

import Data.Coerce (coerce)
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..), stimesMonoid)

import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Prio.Max.Internals as Prio
import Data.PQueue.Internals.Down (Down(..))

import Prelude hiding (null, map, take, drop, takeWhile, dropWhile, splitAt, span, break, (!!), filter)

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
import Data.Data
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

-- | A priority queue with elements of type @a@. Supports extracting the maximum element.
-- Implemented as a wrapper around 'Min.MinQueue'.
newtype MaxQueue a = MaxQ (Min.MinQueue (Down a))
# if __GLASGOW_HASKELL__
  deriving (Eq, Ord, Data)
# else
  deriving (Eq, Ord)
# endif

instance NFData a => NFData (MaxQueue a) where
  rnf (MaxQ q) = rnf q

instance (Ord a, Show a) => Show (MaxQueue a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromDescList " . shows (toDescList xs)

instance Read a => Read (MaxQueue a) where
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

instance Ord a => Semigroup (MaxQueue a) where
  (<>) = union
  stimes = stimesMonoid
  {-# INLINABLE stimes #-}

instance Ord a => Monoid (MaxQueue a) where
  mempty = empty
#if !MIN_VERSION_base(4,11,0)
  mappend = union
#endif
  mconcat = unions

-- | \(O(1)\). The empty priority queue.
empty :: MaxQueue a
empty = MaxQ Min.empty

-- | \(O(1)\). Is this the empty priority queue?
null :: MaxQueue a -> Bool
null (MaxQ q) = Min.null q

-- | \(O(1)\). The number of elements in the queue.
size :: MaxQueue a -> Int
size (MaxQ q) = Min.size q

-- | \(O(1)\). Returns the maximum element of the queue. Throws an error on an empty queue.
findMax :: MaxQueue a -> a
findMax = fromMaybe (error "Error: findMax called on empty queue") . getMax

-- | \(O(1)\). The top (maximum) element of the queue, if there is one.
getMax :: MaxQueue a -> Maybe a
getMax = coerce Min.getMin

-- | \(O(\log n)\). Deletes the maximum element of the queue. Does nothing on an empty queue.
deleteMax :: Ord a => MaxQueue a -> MaxQueue a
deleteMax = coerce Min.deleteMin

-- | \(O(\log n)\). Extracts the maximum element of the queue. Throws an error on an empty queue.
deleteFindMax :: Ord a => MaxQueue a -> (a, MaxQueue a)
deleteFindMax = fromMaybe (error "Error: deleteFindMax called on empty queue") . maxView

-- | \(O(\log n)\). Extract the top (maximum) element of the sequence, if there is one.
maxView :: Ord a => MaxQueue a -> Maybe (a, MaxQueue a)
maxView = coerce Min.minView

-- | \(O(\log n)\). Delete the top (maximum) element of the sequence, if there is one.
delete :: Ord a => MaxQueue a -> Maybe (MaxQueue a)
delete = fmap snd . maxView

-- | \(O(1)\). Construct a priority queue with a single element.
singleton :: a -> MaxQueue a
singleton = coerce Min.singleton

-- | \(O(1)\). Insert an element into the priority queue.
insert :: Ord a => a -> MaxQueue a -> MaxQueue a
insert = coerce Min.insert

-- | \(O(\log min(n_1,n_2))\). Take the union of two priority queues.
union :: Ord a => MaxQueue a -> MaxQueue a -> MaxQueue a
MaxQ q1 `union` MaxQ q2 = MaxQ (q1 `Min.union` q2)

-- | Takes the union of a list of priority queues. Equivalent to @'foldl' 'union' 'empty'@.
unions :: Ord a => [MaxQueue a] -> MaxQueue a
unions = coerce Min.unions

-- | \(O(k \log n)\). Returns the @(k+1)@th largest element of the queue.
(!!) :: Ord a => MaxQueue a -> Int -> a
(!!) = coerce (Min.!!)

{-# INLINE take #-}
-- | \(O(k \log n)\). Returns the list of the @k@ largest elements of the queue, in descending order, or
-- all elements of the queue, if @k >= n@.
take :: Ord a => Int -> MaxQueue a -> [a]
take = coerce Min.take

-- | \(O(k \log n)\). Returns the queue with the @k@ largest elements deleted, or the empty queue if @k >= n@.
drop :: Ord a => Int -> MaxQueue a -> MaxQueue a
drop = coerce Min.drop

-- | \(O(k \log n)\). Equivalent to @(take k queue, drop k queue)@.
splitAt :: Ord a => Int -> MaxQueue a -> ([a], MaxQueue a)
splitAt = coerce Min.splitAt

-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: Ord a => (a -> Bool) -> MaxQueue a -> [a]
takeWhile = coerce Min.takeWhile

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
dropWhile = coerce Min.dropWhile

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
--
span :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
span = coerce Min.span

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
break p = span (not . p)

-- | \(O(n)\). Returns a queue of those elements which satisfy the predicate.
filter :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
filter = coerce Min.filter

-- | \(O(n)\). Returns a pair of queues, where the left queue contains those elements that satisfy the predicate,
-- and the right queue contains those that do not.
partition :: Ord a => (a -> Bool) -> MaxQueue a -> (MaxQueue a, MaxQueue a)
partition = coerce Min.partition

-- | \(O(n)\). Maps a function over the elements of the queue, and collects the 'Just' values.
mapMaybe :: Ord b => (a -> Maybe b) -> MaxQueue a -> MaxQueue b
mapMaybe = coerce Min.mapMaybe

-- | \(O(n)\). Maps a function over the elements of the queue, and separates the 'Left' and 'Right' values.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MaxQueue a -> (MaxQueue b, MaxQueue c)
mapEither = coerce Min.mapEither

-- | \(O(n)\). Creates a new priority queue containing the images of the elements of this queue.
-- Equivalent to @'fromList' . 'Data.List.map' f . toList@.
map :: Ord b => (a -> b) -> MaxQueue a -> MaxQueue b
map = coerce Min.map

-- | \(O(n)\). Assumes that the function it is given is (weakly) monotonic
-- (meaning that @x <= y@ implies @f x <= f y@), and
-- applies this function to every element of the priority queue, as in 'fmap'.
-- If the function is not monotonic, the result is undefined.
mapMonotonic :: (a -> b) -> MaxQueue a -> MaxQueue b
mapMonotonic f (MaxQ q) = MaxQ (Min.mapMonotonic (\(Down a) -> Down (f a)) q)

{-# DEPRECATED mapU "use mapMonotonic instead" #-}
mapU :: (a -> b) -> MaxQueue a -> MaxQueue b
mapU = mapMonotonic

-- | \(O(n)\). Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> MaxQueue a -> b
foldrU f z (MaxQ q) = Min.foldrU (coerce f) z q

-- | \(O(n)\). Unordered monoidal fold on a priority queue.
--
-- @since 1.4.2
foldMapU :: Monoid m => (a -> m) -> MaxQueue a -> m
foldMapU f (MaxQ q) = Min.foldMapU (coerce f) q

-- | \(O(n)\). Unordered left fold on a priority queue. This is rarely
-- what you want; 'foldrU' and 'foldlU'' are more likely to perform
-- well.
foldlU :: (b -> a -> b) -> b -> MaxQueue a -> b
foldlU f z (MaxQ q) = Min.foldlU (coerce f) z q

-- | \(O(n)\). Unordered strict left fold on a priority queue.
--
-- @since 1.4.2
foldlU' :: (b -> a -> b) -> b -> MaxQueue a -> b
foldlU' f z (MaxQ q) = Min.foldlU' (coerce f) z q

{-# INLINE elemsU #-}
-- | Equivalent to 'toListU'.
elemsU :: MaxQueue a -> [a]
elemsU = toListU

{-# INLINE toListU #-}
-- | \(O(n)\). Returns a list of the elements of the priority queue, in no particular order.
toListU :: MaxQueue a -> [a]
toListU = coerce Min.toListU

-- | \(O(n \log n)\). Performs a right-fold on the elements of a priority queue in ascending order.
-- @'foldrAsc' f z q == 'foldlDesc' (flip f) z q@.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrAsc = foldlDesc . flip

-- | \(O(n \log n)\). Performs a left-fold on the elements of a priority queue in descending order.
-- @'foldlAsc' f z q == 'foldrDesc' (flip f) z q@.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlAsc = foldrDesc . flip

-- | \(O(n \log n)\). Performs a right-fold on the elements of a priority queue in descending order.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrDesc f z (MaxQ q) = Min.foldrAsc (coerce f) z q

-- | \(O(n \log n)\). Performs a left-fold on the elements of a priority queue in descending order.
foldlDesc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlDesc f z (MaxQ q) = Min.foldlAsc (coerce f) z q

{-# INLINE toAscList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in ascending order.
toAscList :: Ord a => MaxQueue a -> [a]
toAscList q = build (\c nil -> foldrAsc c nil q)
-- I can see no particular reason this does not simply forward to Min.toDescList. (lsp, 2016)

{-# INLINE toDescList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in descending order.
toDescList :: Ord a => MaxQueue a -> [a]
toDescList q = build (\c nil -> foldrDesc c nil q)
-- I can see no particular reason this does not simply forward to Min.toAscList. (lsp, 2016)

{-# INLINE toList #-}
-- | \(O(n \log n)\). Returns the elements of the priority queue in descending order. Equivalent to 'toDescList'.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: Ord a => MaxQueue a -> [a]
toList = coerce Min.toList

{-# INLINE fromAscList #-}
-- | \(O(n)\). Constructs a priority queue from an ascending list. /Warning/: Does not check the precondition.
fromAscList :: [a] -> MaxQueue a
fromAscList = coerce Min.fromDescList

{-# INLINE fromDescList #-}
-- | \(O(n)\). Constructs a priority queue from a descending list. /Warning/: Does not check the precondition.
fromDescList :: [a] -> MaxQueue a
fromDescList = coerce Min.fromAscList

{-# INLINE fromList #-}
-- | \(O(n \log n)\). Constructs a priority queue from an unordered list.
fromList :: Ord a => [a] -> MaxQueue a
fromList = coerce Min.fromList

-- | \(O(n)\). Constructs a priority queue from the keys of a 'Prio.MaxPQueue'.
keysQueue :: Prio.MaxPQueue k a -> MaxQueue k
keysQueue = coerce Min.keysQueue

-- | \(O(\log n)\). @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MaxQueue' is stored somewhat lazily. In earlier
-- versions of this package, some operations could produce chains of thunks
-- along the spine, occasionally necessitating manual forcing. Now, all
-- operations are careful to force enough to avoid this problem.
{-# DEPRECATED seqSpine "This function is no longer necessary or useful." #-}
seqSpine :: MaxQueue a -> b -> b
seqSpine (MaxQ q) = Min.seqSpine q
