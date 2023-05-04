{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module BinomialQueue.Internals (
  MinQueue (..),
  BinomHeap,
  BinomForest(..),
  BinomTree(..),
  Extract(..),
  MExtract(..),
  Succ(..),
  Zero(..),
  empty,
  extractHeap,
  null,
  size,
  getMin,
  minView,
  singleton,
  insert,
  insertEager,
  union,
  unionPlusOne,
  mapMaybe,
  mapEither,
  mapMonotonic,
  foldrAsc,
  foldlAsc,
  foldrDesc,
  foldrUnfold,
  foldlUnfold,
  insertMinQ,
  insertMinQ',
  insertMaxQ',
  toAscList,
  toDescList,
  toListU,
  fromList,
  mapU,
  fromAscList,
  foldMapU,
  foldrU,
  foldlU,
  foldlU',
  seqSpine,
  unions
  ) where

import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Foldable (foldl')
import Data.Function (on)
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..), stimesMonoid)
#endif

import Data.PQueue.Internals.Foldable
#ifdef __GLASGOW_HASKELL__
import Data.Data
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
import GHC.Exts (build)
#endif

import Prelude hiding (null)

#ifndef __GLASGOW_HASKELL__
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

-- | A priority queue with elements of type @a@. Getting the
-- size or retrieving the minimum element takes \(O(\log n)\) time.
newtype MinQueue a = MinQueue (BinomHeap a)

#ifdef __GLASGOW_HASKELL__
instance (Ord a, Data a) => Data (MinQueue a) where
  gfoldl f z q = case minView q of
    Nothing      -> z empty
    Just (x, q') -> z insert `f` x `f` q'

  gunfold k z c = case constrIndex c of
    1 -> z empty
    2 -> k (k (z insertMinQ))
    _ -> error "gunfold"

  dataCast1 x = gcast1 x

  toConstr q
    | null q = emptyConstr
    | otherwise = consConstr

  dataTypeOf _ = queueDataType

queueDataType :: DataType
queueDataType = mkDataType "Data.PQueue.Min.MinQueue" [emptyConstr, consConstr]

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr queueDataType "empty" [] Prefix
consConstr  = mkConstr queueDataType "<|" [] Infix

#endif

type BinomHeap = BinomForest Zero

instance Ord a => Eq (MinQueue a) where
  (==) = (==) `on` minView

instance Ord a => Ord (MinQueue a) where
  compare = compare `on` minView
    -- We compare their first elements, then their other elements up to the smaller queue's length,
    -- and then the longer queue wins.
    -- This is equivalent to @comparing toAscList@, except it fuses much more nicely.

-- We implement tree ranks in the type system with a nicely elegant approach, as follows.
-- The goal is to have the type system automatically guarantee that our binomial forest
-- has the correct binomial structure.
--
-- In the traditional set-theoretic construction of the natural numbers, we define
-- each number to be the set of numbers less than it, and Zero to be the empty set,
-- as follows:
--
-- 0 = {}  1 = {0}    2 = {0, 1}  3={0, 1, 2} ...
--
-- Binomial trees have a similar structure: a tree of rank @k@ has one child of each
-- rank less than @k@. Let's define the type @rk@ corresponding to rank @k@ to refer
-- to a collection of binomial trees of ranks @0..k-1@. Then we can say that
--
-- > data Succ rk a = Succ (BinomTree rk a) (rk a)
--
-- and this behaves exactly as the successor operator for ranks should behave. Furthermore,
-- we immediately obtain that
--
-- > data BinomTree rk a = BinomTree a (rk a)
--
-- which is nice and compact. With this construction, things work out extremely nicely:
--
-- > BinomTree (Succ (Succ (Succ Zero)))
--
-- is a type constructor that takes an element type and returns the type of binomial trees
-- of rank @3@.
--
-- The Skip constructor must be lazy to obtain the desired amortized bounds.
-- The forest field of the Cons constructor /could/ be made strict, but that
-- would be worse for heavily persistent use. According to our benchmarks, it
-- doesn't make a significant or consistent difference even in non-persistent
-- code (heap sort and k-way merge).
--
-- Debit invariant:
--
-- The next-pointer of a Skip node is allowed 1 debit. No other debits are
-- allowed in the structure.
data BinomForest rk a
   = Nil
   | Skip (BinomForest (Succ rk) a)
   | Cons {-# UNPACK #-} !(BinomTree rk a) (BinomForest (Succ rk) a)

-- The BinomTree and Succ constructors are entirely strict, primarily because
-- that makes it easier to make sure everything is as strict as it should
-- be. The downside is that this slows down `mapMonotonic`. If that's important,
-- we can do all the forcing manually; it will be a pain.

data BinomTree rk a = BinomTree !a !(rk a)

-- | If |rk| corresponds to rank @k@, then |'Succ' rk| corresponds to rank @k+1@.
data Succ rk a = Succ {-# UNPACK #-} !(BinomTree rk a) !(rk a)

-- | Type corresponding to the Zero rank.
data Zero a = Zero

-- basics

-- | \(O(1)\). The empty priority queue.
empty :: MinQueue a
empty = MinQueue Nil

-- | \(O(1)\). Is this the empty priority queue?
null :: MinQueue a -> Bool
null (MinQueue Nil) = True
null _ = False

-- | \(O(\log n)\). The number of elements in the queue.
size :: MinQueue a -> Int
size (MinQueue hp) = go 0 1 hp
  where
    go :: Int -> Int -> BinomForest rk a -> Int
    go acc rk Nil = rk `seq` acc
    go acc rk (Skip f) = go acc (2 * rk) f
    go acc rk (Cons _t f) = go (acc + rk) (2 * rk) f

-- | \(O(\log n)\). Returns the minimum element of the queue, if the queue is nonempty.
getMin :: Ord a => MinQueue a -> Maybe a
-- TODO: Write this directly to avoid rebuilding the heap.
getMin xs = case minView xs of
  Just (a, _) -> Just a
  Nothing -> Nothing

-- | Retrieves the minimum element of the queue, and the queue stripped of that element,
-- or 'Nothing' if passed an empty queue.
minView :: Ord a => MinQueue a -> Maybe (a, MinQueue a)
minView (MinQueue ts) = case extractBin ts of
  No -> Nothing
  Yes (Extract x ~Zero ts') -> Just (x, MinQueue ts')

-- | \(O(1)\). Construct a priority queue with a single element.
singleton :: a -> MinQueue a
singleton x = MinQueue (Cons (tip x) Nil)

-- | Amortized \(O(1)\), worst-case \(O(\log n)\). Insert an element into the priority queue.
insert :: Ord a => a -> MinQueue a -> MinQueue a
insert x (MinQueue ts) = MinQueue (incr (tip x) ts)

-- | \(O(\log n)\), but a fast \(O(1)\) average when inserting repeatedly in
-- an empty queue or at least around \(O(\log n)\) times into a nonempty one.
-- Insert an element into the priority queue. This is good for 'fromList'-like
-- operations.
insertEager :: Ord a => a -> MinQueue a -> MinQueue a
insertEager x (MinQueue ts) = MinQueue (incr' (tip x) ts)
{-# INLINE insertEager #-}

-- | Amortized \(O(\log \min(n,m))\), worst-case \(O(\log \max(n,m))\). Take the union of two priority queues.
union :: Ord a => MinQueue a -> MinQueue a -> MinQueue a
union (MinQueue f1) (MinQueue f2) = MinQueue (merge f1 f2)

-- | Takes the union of a list of priority queues. Equivalent to @'foldl'' 'union' 'empty'@.
unions :: Ord a => [MinQueue a] -> MinQueue a
unions = foldl' union empty

-- | \(O(n)\). Map elements and collect the 'Just' results.
mapMaybe :: Ord b => (a -> Maybe b) -> MinQueue a -> MinQueue b
mapMaybe f = flip foldlU' empty $ \q a ->
  case f a of
    Nothing -> q
    Just b -> insertEager b q
-- This seems to be needed for specialization.
{-# INLINABLE mapMaybe #-}

-- | \(O(n)\). Map elements and separate the 'Left' and 'Right' results.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MinQueue a -> (MinQueue b, MinQueue c)
mapEither f = fromPartition .
  foldlU'
    (\(Partition ls rs) a ->
        case f a of
          Left b -> Partition (insertEager b ls) rs
          Right b -> Partition ls (insertEager b rs))
    (Partition empty empty)
-- This seems to be needed for specialization.
{-# INLINABLE mapEither #-}

-- | \(O(n)\). Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue,
-- as in 'fmap'. If it is not, the result is undefined.
mapMonotonic :: (a -> b) -> MinQueue a -> MinQueue b
mapMonotonic = mapU

{-# INLINABLE [0] foldrAsc #-}
-- | \(O(n \log n)\). Performs a right fold on the elements of a priority queue in
-- ascending order.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrAsc f z (MinQueue ts) = foldrUnfold f z extractHeap ts

-- | \(O(n \log n)\). Performs a right fold on the elements of a priority queue in descending order.
-- @foldrDesc f z q == foldlAsc (flip f) z q@.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrDesc = foldlAsc . flip
{-# INLINE [0] foldrDesc #-}

{-# INLINE foldrUnfold #-}
-- | Equivalent to @foldr f z (unfoldr suc s0)@.
foldrUnfold :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
foldrUnfold f z suc s0 = unf s0 where
  unf s = case suc s of
    Nothing      -> z
    Just (x, s') -> x `f` unf s'

-- | \(O(n \log n)\). Performs a left fold on the elements of a priority queue in
-- ascending order.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MinQueue a -> b
foldlAsc f z (MinQueue ts) = foldlUnfold f z extractHeap ts

{-# INLINE foldlUnfold #-}
-- | @foldlUnfold f z suc s0@ is equivalent to @foldl f z (unfoldr suc s0)@.
foldlUnfold :: (c -> a -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
foldlUnfold f z0 suc s0 = unf z0 s0 where
  unf z s = case suc s of
    Nothing      -> z
    Just (x, s') -> unf (z `f` x) s'

{-# INLINABLE [1] toAscList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in ascending order.
toAscList :: Ord a => MinQueue a -> [a]
toAscList queue = foldrAsc (:) [] queue

{-# INLINABLE toAscListApp #-}
toAscListApp :: Ord a => MinQueue a -> [a] -> [a]
toAscListApp (MinQueue ts) app = foldrUnfold (:) app extractHeap ts

{-# INLINABLE [1] toDescList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in descending order.
toDescList :: Ord a => MinQueue a -> [a]
toDescList queue = foldrDesc (:) [] queue

{-# INLINABLE toDescListApp #-}
toDescListApp :: Ord a => MinQueue a -> [a] -> [a]
toDescListApp (MinQueue ts) app = foldlUnfold (flip (:)) app extractHeap ts

{-# RULES
"toAscList" [~1] forall q. toAscList q = build (\c nil -> foldrAsc c nil q)
"toDescList" [~1] forall q. toDescList q = build (\c nil -> foldrDesc c nil q)
"ascList" [1] forall q add. foldrAsc (:) add q = toAscListApp q add
"descList" [1] forall q add. foldrDesc (:) add q = toDescListApp q add
 #-}

{-# INLINE fromAscList #-}
-- | \(O(n)\). Constructs a priority queue from an ascending list. /Warning/: Does not check the precondition.
--
-- Performance note: Code using this function in a performance-sensitive context
-- with an argument that is a "good producer" for list fusion should be compiled
-- with @-fspec-constr@ or @-O2@. For example, @fromAscList . map f@ needs one
-- of these options for best results.
fromAscList :: [a] -> MinQueue a
-- We apply an explicit argument to get foldl' to inline.
fromAscList xs = foldl' (flip insertMaxQ') empty xs

-- | Takes a size and a binomial forest and produces a priority queue with a distinguished global root.
extractHeap :: Ord a => BinomHeap a -> Maybe (a, BinomHeap a)
extractHeap ts = case extractBin ts of
  No                        -> Nothing
  Yes (Extract x ~Zero ts') -> Just (x, ts')

-- | A specialized type intended to organize the return of extract-min queries
-- from a binomial forest. We walk all the way through the forest, and then
-- walk backwards. @Extract rk a@ is the result type of an extract-min
-- operation that has walked as far backwards of rank @rk@ -- that is, it
-- has visited every root of rank @>= rk@.
--
-- The interpretation of @Extract minKey children forest@ is
--
--   * @minKey@ is the key of the minimum root visited so far. It may have
--     any rank @>= rk@. We will denote the root corresponding to
--     @minKey@ as @minRoot@.
--
--   * @children@ is those children of @minRoot@ which have not yet been
--     merged with the rest of the forest. Specifically, these are
--     the children with rank @< rk@.
--
--   * @forest@ is an accumulating parameter that maintains the partial
--     reconstruction of the binomial forest without @minRoot@. It is
--     the union of all old roots with rank @>= rk@ (except @minRoot@),
--     with the set of all children of @minRoot@ with rank @>= rk@.
data Extract rk a = Extract !a !(rk a) !(BinomForest rk a)
data MExtract rk a = No | Yes {-# UNPACK #-} !(Extract rk a)

incrExtract :: Extract (Succ rk) a -> Extract rk a
incrExtract (Extract minKey (Succ kChild kChildren) ts)
  = Extract minKey kChildren (Cons kChild ts)

-- Note: We used to apply Skip lazily here, and to use the lazy incr, for fear
-- that the potential cascade of carries would be more expensive than leaving
-- those carries suspended and letting subsequent operations force them.
-- However, our benchmarks indicated that doing these strictly was
-- faster. Note that even if we chose to go back to incr (rather than incr'),
-- it's even more clearly worse to apply Skip lazily— forcing the result of
-- incr in this context doesn't cause a cascade, because the child of any Cons
-- will come from an Extract, and therefore be in WHNF already.
incrExtract' :: Ord a => BinomTree rk a -> Extract (Succ rk) a -> Extract rk a
incrExtract' t (Extract minKey (Succ kChild kChildren) ts)
  = Extract minKey kChildren (Skip $! incr' (t `joinBin` kChild) ts)

-- | Walks backward from the biggest key in the forest, as far as rank @rk@.
-- Returns its progress. Each successive application of @extractBin@ takes
-- amortized \(O(1)\) time, so applying it from the beginning takes \(O(\log n)\) time.
extractBin :: Ord a => BinomForest rk a -> MExtract rk a
extractBin = start
  where
    start :: Ord a => BinomForest rk a -> MExtract rk a
    start Nil = No
    start (Skip f) = case start f of
      No     -> No
      Yes ex -> Yes (incrExtract ex)
    start (Cons t@(BinomTree x ts) f) = Yes $ case go x f of
      No -> Extract x ts (skip f)
      Yes ex -> incrExtract' t ex

    go :: Ord a => a -> BinomForest rk a -> MExtract rk a
    go _min_above Nil = _min_above `seq` No
    go min_above (Skip f) = case go min_above f of
      No -> No
      Yes ex -> Yes (incrExtract ex)
    go min_above (Cons t@(BinomTree x ts) f)
      | min_above <= x = case go min_above f of
          No -> No
          Yes ex -> Yes (incrExtract' t ex)
      | otherwise = case go x f of
          No -> Yes (Extract x ts (skip f))
          Yes ex -> Yes (incrExtract' t ex)

-- | When the heap size is a power of two and we extract from it, we have
-- to shrink the spine by one. This function takes care of that.
skip :: BinomForest (Succ rk) a -> BinomForest rk a
skip Nil = Nil
skip f = Skip f
{-# INLINE skip #-}

data Partition a b = Partition !(MinQueue a) !(MinQueue b)
fromPartition :: Partition a b -> (MinQueue a, MinQueue b)
fromPartition (Partition p q) = (p, q)

{-# INLINE tip #-}
-- | Constructs a binomial tree of rank 0.
tip :: a -> BinomTree Zero a
tip x = BinomTree x Zero

insertMinQ :: a -> MinQueue a -> MinQueue a
insertMinQ x (MinQueue f) = MinQueue (insertMin (tip x) f)

-- | @insertMin t f@ assumes that the root of @t@ compares as less than
-- or equal to every other root in @f@, and merges accordingly.
insertMin :: BinomTree rk a -> BinomForest rk a -> BinomForest rk a
insertMin t Nil = Cons t Nil
insertMin t (Skip f) = Cons t f
-- See Note [Force on cascade]
insertMin (BinomTree x ts) (Cons t' f) = f `seq` Skip (insertMin (BinomTree x (Succ t' ts)) f)

-- | @insertMinQ' x h@ assumes that @x@ compares as less
-- than or equal to every element of @h@.
insertMinQ' :: a -> MinQueue a -> MinQueue a
insertMinQ' x (MinQueue f) = MinQueue (insertMin' (tip x) f)

-- | @insertMin' t f@ assumes that the root of @t@ compares as less than
-- every other root in @f@, and merges accordingly. It eagerly evaluates
-- the modified portion of the structure.
insertMin' :: BinomTree rk a -> BinomForest rk a -> BinomForest rk a
insertMin' t Nil = Cons t Nil
insertMin' t (Skip f) = Cons t f
insertMin' (BinomTree x ts) (Cons t' f) = Skip $! insertMin' (BinomTree x (Succ t' ts)) f

-- | @insertMaxQ' x h@ assumes that @x@ compares as greater
-- than or equal to every element of @h@. It also assumes,
-- and preserves, an extra invariant. See 'insertMax'' for details.
-- tldr: this function can be used safely to build a queue from an
-- ascending list/array/whatever, but that's about it.
insertMaxQ' :: a -> MinQueue a -> MinQueue a
insertMaxQ' x (MinQueue f) = MinQueue (insertMax' (tip x) f)

-- | @insertMax' t f@ assumes that the root of @t@ compares as greater
-- than or equal to every root in @f@, and further assumes that the roots
-- in @f@ occur in descending order. It produces a forest whose roots are
-- again in descending order. Note: the whole modified portion of the spine
-- is forced.
insertMax' :: BinomTree rk a -> BinomForest rk a -> BinomForest rk a
insertMax' t Nil = Cons t Nil
insertMax' t (Skip f) = Cons t f
insertMax' t (Cons (BinomTree x ts) f) = Skip $! insertMax' (BinomTree x (Succ t ts)) f

{-# INLINABLE fromList #-}
-- | \(O(n)\). Constructs a priority queue from an unordered list.
fromList :: Ord a => [a] -> MinQueue a
fromList xs = foldl' (flip insertEager) empty xs

-- | Given two binomial forests starting at rank @rk@, takes their union.
-- Each successive application of this function costs \(O(1)\), so applying it
-- from the beginning costs \(O(\log n)\).
merge :: Ord a => BinomForest rk a -> BinomForest rk a -> BinomForest rk a
merge f1 f2 = case (f1, f2) of
  (Skip f1', Skip f2')    -> Skip $! merge f1' f2'
  (Skip f1', Cons t2 f2') -> Cons t2 $! merge f1' f2'
  (Cons t1 f1', Skip f2') -> Cons t1 $! merge f1' f2'
  (Cons t1 f1', Cons t2 f2')
        -> Skip $! carry (t1 `joinBin` t2) f1' f2'
  (Nil, _)                -> f2
  (_, Nil)                -> f1

-- | Take the union of two queues and toss in an extra element.
unionPlusOne :: Ord a => a -> MinQueue a -> MinQueue a -> MinQueue a
unionPlusOne a (MinQueue xs) (MinQueue ys) = MinQueue (carry (tip a) xs ys)

-- | Merges two binomial forests with another tree. If we are thinking of the trees
-- in the binomial forest as binary digits, this corresponds to a carry operation.
-- Each call to this function takes \(O(1)\) time, so in total, it costs \(O(\log n)\).
carry :: Ord a => BinomTree rk a -> BinomForest rk a -> BinomForest rk a -> BinomForest rk a
carry t0 f1 f2 = t0 `seq` case (f1, f2) of
  (Skip f1', Skip f2')    -> Cons t0 $! merge f1' f2'
  (Skip f1', Cons t2 f2') -> Skip $! mergeCarry t0 t2 f1' f2'
  (Cons t1 f1', Skip f2') -> Skip $! mergeCarry t0 t1 f1' f2'
  (Cons t1 f1', Cons t2 f2')
        -> Cons t0 $! mergeCarry t1 t2 f1' f2'
  -- Why do these use incr and not incr'? We want the merge to take amortized
  -- O(log(min(|f1|, |f2|))) time. If we performed this final increment
  -- eagerly, that would degrade to O(log(max(|f1|, |f2|))) time.
  (Nil, _f2)              -> incr t0 f2
  (_f1, Nil)              -> incr t0 f1
  where
    mergeCarry tA tB = carry (tA `joinBin` tB)

-- | Merges a binomial tree into a binomial forest. If we are thinking
-- of the trees in the binomial forest as binary digits, this corresponds
-- to adding a power of 2. This costs amortized \(O(1)\) time.
incr :: Ord a => BinomTree rk a -> BinomForest rk a -> BinomForest rk a
-- See Note [Amortization]
incr t f0 = t `seq` case f0 of
  Nil  -> Cons t Nil
  Skip f     -> Cons t f
  Cons t' f' -> f' `seq` Skip (incr (t `joinBin` t') f')
      -- See Note [Force on cascade]

      -- Question: should we force t `cat` t' here? We're allowed to;
      -- it's not obviously good or obviously bad.

-- Note [Amortization]
--
-- In the Skip case, we perform O(1) unshared work and pay a
-- debit. In the Cons case, there are no debits on f', so we can force it for
-- free. We perform O(1) unshared work, and by induction suspend O(1) amortized
-- work. Another way to look at this: We have a string of Conses followed by
-- a Skip or Nil. We change all the Conses to Skips, and change the Skip to
-- a Cons or the Nil to a Cons Nil. Processing each Cons takes O(1) time, which
-- we account for by placing debits below the new Skips. Note: this increment
-- pattern is exactly the same as the one for Hinze-Paterson 2–3 finger trees,
-- and the amortization argument works just the same.

-- Note [Force on cascade]
--
-- As Hinze and Patterson noticed in a similar structure, whenever we cascade
-- past a Cons on insertion, we should force its child. If we don't, then
-- multiple insertions in a row will form a chain of thunks just under the root
-- of the structure, which degrades the worst-case bound for deletion from
-- logarithmic to linear and leads to poor real-world performance.

-- | A version of 'incr' that constructs the spine eagerly. This is
-- intended for implementing @fromList@.
incr' :: Ord a => BinomTree rk a -> BinomForest rk a -> BinomForest rk a
incr' t f0 = t `seq` case f0 of
  Nil  -> Cons t Nil
  Skip f     -> Cons t f
  Cons t' f' -> Skip $! incr' (t `joinBin` t') f'

-- | The carrying operation: takes two binomial heaps of the same rank @k@
-- and returns one of rank @k+1@. Takes \(O(1)\) time.
joinBin :: Ord a => BinomTree rk a -> BinomTree rk a -> BinomTree (Succ rk) a
joinBin t1@(BinomTree x1 ts1) t2@(BinomTree x2 ts2)
  | x1 <= x2 = BinomTree x1 (Succ t2 ts1)
  | otherwise  = BinomTree x2 (Succ t1 ts2)


instance Functor Zero where
  fmap _ _ = Zero

instance Functor rk => Functor (Succ rk) where
  fmap f (Succ t ts) = Succ (fmap f t) (fmap f ts)

instance Functor rk => Functor (BinomTree rk) where
  fmap f (BinomTree x ts) = BinomTree (f x) (fmap f ts)

instance Functor rk => Functor (BinomForest rk) where
  fmap _ Nil = Nil
  fmap f (Skip ts) = Skip $! fmap f ts
  fmap f (Cons t ts) = Cons (fmap f t) $! fmap f ts

instance Foldr Zero where
  foldr_ _ z ~Zero = z

instance Foldl Zero where
  foldl_ _ z ~Zero = z

instance Foldl' Zero where
  foldl'_ _ z ~Zero = z

instance FoldMap Zero where
  foldMap_ _ ~Zero = mempty

instance Foldr rk => Foldr (Succ rk) where
  foldr_ f z (Succ t ts) = foldr_ f (foldr_ f z ts) t

instance Foldl rk => Foldl (Succ rk) where
  foldl_ f z (Succ t ts) = foldl_ f (foldl_ f z t) ts

instance Foldl' rk => Foldl' (Succ rk) where
  foldl'_ f !z (Succ t ts) = foldl'_ f (foldl'_ f z t) ts

instance FoldMap rk => FoldMap (Succ rk) where
  foldMap_ f (Succ t ts) = foldMap_ f t `mappend` foldMap_ f ts

instance Foldr rk => Foldr (BinomTree rk) where
  foldr_ f z (BinomTree x ts) = x `f` foldr_ f z ts

instance Foldl rk => Foldl (BinomTree rk) where
  foldl_ f z (BinomTree x ts) = foldl_ f (z `f` x) ts

instance Foldl' rk => Foldl' (BinomTree rk) where
  foldl'_ f !z (BinomTree x ts) = foldl'_ f (z `f` x) ts

instance FoldMap rk => FoldMap (BinomTree rk) where
  foldMap_ f (BinomTree x ts) = f x `mappend` foldMap_ f ts

instance Foldr rk => Foldr (BinomForest rk) where
  foldr_ _ z Nil          = z
  foldr_ f z (Skip tss)   = foldr_ f z tss
  foldr_ f z (Cons t tss) = foldr_ f (foldr_ f z tss) t

instance Foldl rk => Foldl (BinomForest rk) where
  foldl_ _ z Nil          = z
  foldl_ f z (Skip tss)   = foldl_ f z tss
  foldl_ f z (Cons t tss) = foldl_ f (foldl_ f z t) tss

instance Foldl' rk => Foldl' (BinomForest rk) where
  foldl'_ _ !z Nil          = z
  foldl'_ f !z (Skip tss)   = foldl'_ f z tss
  foldl'_ f !z (Cons t tss) = foldl'_ f (foldl'_ f z t) tss

instance FoldMap rk => FoldMap (BinomForest rk) where
  foldMap_ _ Nil = mempty
  foldMap_ f (Skip tss)   = foldMap_ f tss
  foldMap_ f (Cons t tss) = foldMap_ f t `mappend` foldMap_ f tss

{-
instance Foldable Zero where
  foldr _ z ~Zero = z
  foldl _ z ~Zero = z

instance Foldable rk => Foldable (Succ rk) where
  foldr f z (Succ t ts) = foldr f (foldr f z ts) t
  foldl f z (Succ t ts) = foldl f (foldl f z t) ts

instance Foldable rk => Foldable (BinomTree rk) where
  foldr f z (BinomTree x ts) = x `f` foldr f z ts
  foldl f z (BinomTree x ts) = foldl f (z `f` x) ts

instance Foldable rk => Foldable (BinomForest rk) where
  foldr _ z Nil          = z
  foldr f z (Skip tss)   = foldr f z tss
  foldr f z (Cons t tss) = foldr f (foldr f z tss) t
  foldl _ z Nil          = z
  foldl f z (Skip tss)   = foldl f z tss
  foldl f z (Cons t tss) = foldl f (foldl f z t) tss
-}

-- instance Traversable Zero where
--   traverse _ _ = pure Zero
--
-- instance Traversable rk => Traversable (Succ rk) where
--   traverse f (Succ t ts) = Succ <$> traverse f t <*> traverse f ts
--
-- instance Traversable rk => Traversable (BinomTree rk) where
--   traverse f (BinomTree x ts) = BinomTree <$> f x <*> traverse f ts
--
-- instance Traversable rk => Traversable (BinomForest rk) where
--   traverse _ Nil = pure Nil
--   traverse f (Skip tss) = Skip <$> traverse f tss
--   traverse f (Cons t tss) = Cons <$> traverse f t <*> traverse f tss

mapU :: (a -> b) -> MinQueue a -> MinQueue b
mapU f (MinQueue ts) = MinQueue (f <$> ts)

{-# NOINLINE [0] foldrU #-}
-- | \(O(n)\). Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> MinQueue a -> b
foldrU f z (MinQueue ts) = foldr_ f z ts

-- | \(O(n)\). Unordered left fold on a priority queue. This is rarely
-- what you want; 'foldrU' and 'foldlU'' are more likely to perform
-- well.
foldlU :: (b -> a -> b) -> b -> MinQueue a -> b
foldlU f z (MinQueue ts) = foldl_ f z ts

-- | \(O(n)\). Unordered strict left fold on a priority queue.
--
-- @since 1.4.2
foldlU' :: (b -> a -> b) -> b -> MinQueue a -> b
foldlU' f z (MinQueue ts) = foldl'_ f z ts

-- | \(O(n)\). Unordered monoidal fold on a priority queue.
--
-- @since 1.4.2
foldMapU :: Monoid m => (a -> m) -> MinQueue a -> m
foldMapU f (MinQueue ts) = foldMap_ f ts

{-# NOINLINE toListU #-}
-- | \(O(n)\). Returns the elements of the queue, in no particular order.
toListU :: MinQueue a -> [a]
toListU q = foldrU (:) [] q

{-# NOINLINE toListUApp #-}
toListUApp :: MinQueue a -> [a] -> [a]
toListUApp (MinQueue ts) app = foldr_ (:) app ts

{-# RULES
"toListU/build" [~1] forall q. toListU q = build (\c n -> foldrU c n q)
"toListU" [1] forall q app. foldrU (:) app q = toListUApp q app
  #-}

-- traverseU :: Applicative f => (a -> f b) -> MinQueue a -> f (MinQueue b)
-- traverseU _ Empty = pure Empty
-- traverseU f (MinQueue n x ts) = MinQueue n <$> f x <*> traverse f ts

-- | \(O(\log n)\). @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MinQueue' is stored somewhat lazily. Most operations
-- take great care to prevent chains of thunks from accumulating along the
-- spine to the detriment of performance. However, @mapU@ can leave expensive
-- thunks in the structure and repeated applications of that function can
-- create thunk chains.
seqSpine :: MinQueue a -> b -> b
seqSpine (MinQueue ts) z = seqSpineF ts z

seqSpineF :: BinomForest rk a -> b -> b
seqSpineF Nil z          = z
seqSpineF (Skip ts') z   = seqSpineF ts' z
seqSpineF (Cons _ ts') z = seqSpineF ts' z

class NFRank rk where
  rnfRk :: NFData a => rk a -> ()

instance NFRank Zero where
  rnfRk _ = ()

instance NFRank rk => NFRank (Succ rk) where
  rnfRk (Succ t ts) = t `deepseq` rnfRk ts

instance (NFData a, NFRank rk) => NFData (BinomTree rk a) where
  rnf (BinomTree x ts) = x `deepseq` rnfRk ts

instance (NFData a, NFRank rk) => NFData (BinomForest rk a) where
  rnf Nil         = ()
  rnf (Skip ts)   = rnf ts
  rnf (Cons t ts) = t `deepseq` rnf ts

instance NFData a => NFData (MinQueue a) where
  rnf (MinQueue ts) = rnf ts

instance (Ord a, Show a) => Show (MinQueue a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromAscList " . shows (toAscList xs)

instance Read a => Read (MinQueue a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromAscList" <- lexP
    xs <- readPrec
    return (fromAscList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromAscList",s) <- lex r
    (xs,t) <- reads s
    return (fromAscList xs,t)
#endif

#if MIN_VERSION_base(4,9,0)
instance Ord a => Semigroup (MinQueue a) where
  (<>) = union
  stimes = stimesMonoid
  {-# INLINABLE stimes #-}
#endif

instance Ord a => Monoid (MinQueue a) where
  mempty = empty
#if !MIN_VERSION_base(4,11,0)
  mappend = union
#endif
  mconcat = unions
