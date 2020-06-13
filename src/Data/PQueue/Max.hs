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
-- The spine of the heap is maintained lazily. To force the spine of the heap,
-- use 'seqSpine'.
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
  elemsU,
  toListU,
  -- * Miscellaneous operations
  keysQueue,
  seqSpine) where

import Control.DeepSeq (NFData(rnf))

import Data.Functor ((<$>))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl, foldr)

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif

import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Prio.Max.Internals as Prio
import Data.PQueue.Prio.Max.Internals (Down(..))

import Prelude hiding (null, foldr, foldl, take, drop, takeWhile, dropWhile, splitAt, span, break, (!!), filter)

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
  deriving (Eq, Ord, Data, Typeable)
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

#if MIN_VERSION_base(4,9,0)
instance Ord a => Semigroup (MaxQueue a) where
  (<>) = union
#endif

instance Ord a => Monoid (MaxQueue a) where
  mempty = empty
  mappend = union

-- | /O(1)/. The empty priority queue.
empty :: MaxQueue a
empty = MaxQ Min.empty

-- | /O(1)/. Is this the empty priority queue?
null :: MaxQueue a -> Bool
null (MaxQ q) = Min.null q

-- | /O(1)/. The number of elements in the queue.
size :: MaxQueue a -> Int
size (MaxQ q) = Min.size q

-- | /O(1)/. Returns the maximum element of the queue. Throws an error on an empty queue.
findMax :: MaxQueue a -> a
findMax = fromMaybe (error "Error: findMax called on empty queue") . getMax

-- | /O(1)/. The top (maximum) element of the queue, if there is one.
getMax :: MaxQueue a -> Maybe a
getMax (MaxQ q) = unDown <$> Min.getMin q

-- | /O(log n)/. Deletes the maximum element of the queue. Does nothing on an empty queue.
deleteMax :: Ord a => MaxQueue a -> MaxQueue a
deleteMax (MaxQ q) = MaxQ (Min.deleteMin q)

-- | /O(log n)/. Extracts the maximum element of the queue. Throws an error on an empty queue.
deleteFindMax :: Ord a => MaxQueue a -> (a, MaxQueue a)
deleteFindMax = fromMaybe (error "Error: deleteFindMax called on empty queue") . maxView

-- | /O(log n)/. Extract the top (maximum) element of the sequence, if there is one.
maxView :: Ord a => MaxQueue a -> Maybe (a, MaxQueue a)
maxView (MaxQ q) = case Min.minView q of
  Nothing -> Nothing
  Just (Down x, q')
          -> Just (x, MaxQ q')

-- | /O(log n)/. Delete the top (maximum) element of the sequence, if there is one.
delete :: Ord a => MaxQueue a -> Maybe (MaxQueue a)
delete = fmap snd . maxView

-- | /O(1)/. Construct a priority queue with a single element.
singleton :: a -> MaxQueue a
singleton = MaxQ . Min.singleton . Down

-- | /O(1)/. Insert an element into the priority queue.
insert :: Ord a => a -> MaxQueue a -> MaxQueue a
x `insert` MaxQ q = MaxQ (Down x `Min.insert` q)

-- | /O(log (min(n1,n2)))/. Take the union of two priority queues.
union :: Ord a => MaxQueue a -> MaxQueue a -> MaxQueue a
MaxQ q1 `union` MaxQ q2 = MaxQ (q1 `Min.union` q2)

-- | Takes the union of a list of priority queues. Equivalent to @'foldl' 'union' 'empty'@.
unions :: Ord a => [MaxQueue a] -> MaxQueue a
unions qs = MaxQ (Min.unions [q | MaxQ q <- qs])

-- | /O(k log n)/. Returns the @(k+1)@th largest element of the queue.
(!!) :: Ord a => MaxQueue a -> Int -> a
MaxQ q !! n = unDown ((Min.!!) q n)

{-# INLINE take #-}
-- | /O(k log n)/. Returns the list of the @k@ largest elements of the queue, in descending order, or
-- all elements of the queue, if @k >= n@.
take :: Ord a => Int -> MaxQueue a -> [a]
take k (MaxQ q) = [a | Down a <- Min.take k q]

-- | /O(k log n)/. Returns the queue with the @k@ largest elements deleted, or the empty queue if @k >= n@.
drop :: Ord a => Int -> MaxQueue a -> MaxQueue a
drop k (MaxQ q) = MaxQ (Min.drop k q)

-- | /O(k log n)/. Equivalent to @(take k queue, drop k queue)@.
splitAt :: Ord a => Int -> MaxQueue a -> ([a], MaxQueue a)
splitAt k (MaxQ q) = (map unDown xs, MaxQ q') where
  (xs, q') = Min.splitAt k q

-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: Ord a => (a -> Bool) -> MaxQueue a -> [a]
takeWhile p (MaxQ q) = map unDown (Min.takeWhile (p . unDown) q)

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
dropWhile p (MaxQ q) = MaxQ (Min.dropWhile (p . unDown) q)

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
--
span :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
span p (MaxQ q) = (map unDown xs, MaxQ q') where
  (xs, q') = Min.span (p . unDown) q

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: Ord a => (a -> Bool) -> MaxQueue a -> ([a], MaxQueue a)
break p = span (not . p)

-- | /O(n)/. Returns a queue of those elements which satisfy the predicate.
filter :: Ord a => (a -> Bool) -> MaxQueue a -> MaxQueue a
filter p (MaxQ q) = MaxQ (Min.filter (p . unDown) q)

-- | /O(n)/. Returns a pair of queues, where the left queue contains those elements that satisfy the predicate,
-- and the right queue contains those that do not.
partition :: Ord a => (a -> Bool) -> MaxQueue a -> (MaxQueue a, MaxQueue a)
partition p (MaxQ q) = (MaxQ q0, MaxQ q1)
  where  (q0, q1) = Min.partition (p . unDown) q

-- | /O(n)/. Maps a function over the elements of the queue, and collects the 'Just' values.
mapMaybe :: Ord b => (a -> Maybe b) -> MaxQueue a -> MaxQueue b
mapMaybe f (MaxQ q) = MaxQ (Min.mapMaybe (\(Down x) -> Down <$> f x) q)

-- | /O(n)/. Maps a function over the elements of the queue, and separates the 'Left' and 'Right' values.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MaxQueue a -> (MaxQueue b, MaxQueue c)
mapEither f (MaxQ q) = (MaxQ q0, MaxQ q1)
  where  (q0, q1) = Min.mapEither (either (Left . Down) (Right . Down) . f . unDown) q

-- | /O(n)/. Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue.
-- /Does not check the precondition/.
mapU :: (a -> b) -> MaxQueue a -> MaxQueue b
mapU f (MaxQ q) = MaxQ (Min.mapU (\(Down a) -> Down (f a)) q)

-- | /O(n)/. Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> MaxQueue a -> b
foldrU f z (MaxQ q) = Min.foldrU (flip (foldr f)) z q

-- | /O(n)/. Unordered left fold on a priority queue.
foldlU :: (b -> a -> b) -> b -> MaxQueue a -> b
foldlU f z (MaxQ q) = Min.foldlU (foldl f) z q

{-# INLINE elemsU #-}
-- | Equivalent to 'toListU'.
elemsU :: MaxQueue a -> [a]
elemsU = toListU

{-# INLINE toListU #-}
-- | /O(n)/. Returns a list of the elements of the priority queue, in no particular order.
toListU :: MaxQueue a -> [a]
toListU (MaxQ q) = map unDown (Min.toListU q)

-- | /O(n log n)/. Performs a right-fold on the elements of a priority queue in ascending order.
-- @'foldrAsc' f z q == 'foldlDesc' (flip f) z q@.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrAsc = foldlDesc . flip

-- | /O(n log n)/. Performs a left-fold on the elements of a priority queue in descending order.
-- @'foldlAsc' f z q == 'foldrDesc' (flip f) z q@.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlAsc = foldrDesc . flip

-- | /O(n log n)/. Performs a right-fold on the elements of a priority queue in descending order.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MaxQueue a -> b
foldrDesc f z (MaxQ q) = Min.foldrAsc (flip (foldr f)) z q

-- | /O(n log n)/. Performs a left-fold on the elements of a priority queue in descending order.
foldlDesc :: Ord a => (b -> a -> b) -> b -> MaxQueue a -> b
foldlDesc f z (MaxQ q) = Min.foldlAsc (foldl f) z q

{-# INLINE toAscList #-}
-- | /O(n log n)/. Extracts the elements of the priority queue in ascending order.
toAscList :: Ord a => MaxQueue a -> [a]
toAscList q = build (\c nil -> foldrAsc c nil q)
-- I can see no particular reason this does not simply forward to Min.toDescList. (lsp, 2016)

{-# INLINE toDescList #-}
-- | /O(n log n)/. Extracts the elements of the priority queue in descending order.
toDescList :: Ord a => MaxQueue a -> [a]
toDescList q = build (\c nil -> foldrDesc c nil q)
-- I can see no particular reason this does not simply forward to Min.toAscList. (lsp, 2016)

{-# INLINE toList #-}
-- | /O(n log n)/. Returns the elements of the priority queue in ascending order. Equivalent to 'toDescList'.
--
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: Ord a => MaxQueue a -> [a]
toList (MaxQ q) = map unDown (Min.toList q)

{-# INLINE fromAscList #-}
-- | /O(n)/. Constructs a priority queue from an ascending list. /Warning/: Does not check the precondition.
fromAscList :: [a] -> MaxQueue a
fromAscList = MaxQ . Min.fromDescList . map Down

{-# INLINE fromDescList #-}
-- | /O(n)/. Constructs a priority queue from a descending list. /Warning/: Does not check the precondition.
fromDescList :: [a] -> MaxQueue a
fromDescList = MaxQ . Min.fromAscList . map Down

{-# INLINE fromList #-}
-- | /O(n log n)/. Constructs a priority queue from an unordered list.
fromList :: Ord a => [a] -> MaxQueue a
fromList = foldr insert empty

-- | /O(n)/. Constructs a priority queue from the keys of a 'Prio.MaxPQueue'.
keysQueue :: Prio.MaxPQueue k a -> MaxQueue k
keysQueue (Prio.MaxPQ q) = MaxQ (Min.keysQueue q)

-- | /O(log n)/. Forces the spine of the heap.
seqSpine :: MaxQueue a -> b -> b
seqSpine (MaxQ q) = Min.seqSpine q
