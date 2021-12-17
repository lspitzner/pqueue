{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.PQueue.Prio.Internals (
  MinPQueue(..),
  BinomForest(..),
  BinomHeap,
  BinomTree(..),
  Zero(..),
  Succ(..),
  CompF,
  empty,
  null,
  size,
  singleton,
  insert,
  insertBehind,
  union,
  getMin,
  adjustMinWithKey,
  updateMinWithKey,
  minViewWithKey,
  mapWithKey,
  mapKeysMonotonic,
  mapMaybeWithKey,
  mapEitherWithKey,
  foldrWithKey,
  foldlWithKey,
  foldrU,
  toAscList,
  toDescList,
  toListU,
  insertMin,
  insertMin',
  insertMax',
  fromList,
  fromAscList,
  foldrWithKeyU,
  foldMapWithKeyU,
  foldlWithKeyU,
  foldlWithKeyU',
  traverseWithKey,
  mapMWithKey,
  traverseWithKeyU,
  seqSpine,
  mapForest,
  unions
  ) where

import Control.Applicative.Identity (Identity(Identity, runIdentity))
import Control.Applicative (liftA2, liftA3)
import Control.DeepSeq (NFData(rnf), deepseq)
import qualified Data.List as List
import Data.PQueue.Internals.Foldable

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..), stimesMonoid)
#else
import Data.Monoid ((<>))
#endif

import Prelude hiding (null, map)
#ifdef __GLASGOW_HASKELL__
import Data.Data
import GHC.Exts (build)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
#endif

#ifndef __GLASGOW_HASKELL__
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

#if __GLASGOW_HASKELL__
instance (Data k, Data a, Ord k) => Data (MinPQueue k a) where
  gfoldl f z m = z fromList `f` foldrWithKey (curry (:)) [] m
  toConstr _   = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _ = queueDataType
  dataCast1 f  = gcast1 f
  dataCast2 f  = gcast2 f

queueDataType :: DataType
queueDataType = mkDataType "Data.PQueue.Prio.Min.MinPQueue" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr queueDataType "fromList" [] Prefix

#endif

#if MIN_VERSION_base(4,9,0)
instance Ord k => Semigroup (MinPQueue k a) where
  (<>) = union
  stimes = stimesMonoid
#endif

instance Ord k => Monoid (MinPQueue k a) where
  mempty = empty
#if !MIN_VERSION_base(4,11,0)
  mappend = union
#endif
  mconcat = unions

instance (Ord k, Show k, Show a) => Show (MinPQueue k a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromAscList " . shows (toAscList xs)

instance (Read k, Read a) => Read (MinPQueue k a) where
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

-- | The union of a list of queues: (@'unions' == 'List.foldl' 'union' 'empty'@).
unions :: Ord k => [MinPQueue k a] -> MinPQueue k a
unions = List.foldl' union empty


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)

second' :: (b -> c) -> (a, b) -> (a, c)
second' f (a, b) = (a, f b)

infixr 8 .:

-- | A priority queue where values of type @a@ are annotated with keys of type @k@.
-- The queue supports extracting the element with minimum key.
data MinPQueue k a = Empty | MinPQ {-# UNPACK #-} !Int !k a !(BinomHeap k a)

data BinomForest rk k a =
  Nil |
  Skip (BinomForest (Succ rk) k a) |
  Cons {-# UNPACK #-} !(BinomTree rk k a) (BinomForest (Succ rk) k a)
type BinomHeap = BinomForest Zero

data BinomTree rk k a = BinomTree !k a !(rk k a)
data Zero k a = Zero
data Succ rk k a = Succ {-# UNPACK #-} !(BinomTree rk k a) !(rk k a)

instance IFoldl' Zero where
  foldlWithKey'_ _ z ~Zero = z

instance IFoldMap Zero where
  foldMapWithKey_ _ ~Zero = mempty

instance IFoldl' t => IFoldl' (Succ t) where
  foldlWithKey'_ f z (Succ t rk) = foldlWithKey'_ f z' rk
    where
      !z' = foldlWithKey'_ f z t

instance IFoldMap t => IFoldMap (Succ t) where
  foldMapWithKey_ f (Succ t rk) = foldMapWithKey_ f t `mappend` foldMapWithKey_ f rk

instance IFoldl' rk => IFoldl' (BinomTree rk) where
  foldlWithKey'_ f !z (BinomTree k a rk) = foldlWithKey'_ f ft rk
    where
      !ft = f z k a

instance IFoldMap rk => IFoldMap (BinomTree rk) where
  foldMapWithKey_ f (BinomTree k a rk) = f k a `mappend` foldMapWithKey_ f rk

instance IFoldl' t => IFoldl' (BinomForest t) where
  foldlWithKey'_ _f z Nil = z
  foldlWithKey'_ f !z (Skip ts) = foldlWithKey'_ f z ts
  foldlWithKey'_ f !z (Cons t ts) = foldlWithKey'_ f ft ts
    where
      !ft = foldlWithKey'_ f z t

instance IFoldMap t => IFoldMap (BinomForest t) where
  foldMapWithKey_ _f Nil = mempty
  foldMapWithKey_ f (Skip ts) = foldMapWithKey_ f ts
  foldMapWithKey_ f (Cons t ts) = foldMapWithKey_ f t `mappend` foldMapWithKey_ f ts

type CompF a = a -> a -> Bool

instance (Ord k, Eq a) => Eq (MinPQueue k a) where
  MinPQ n1 k1 a1 ts1 == MinPQ n2 k2 a2 ts2 =
    n1 == n2 && eqExtract k1 a1 ts1 k2 a2 ts2
  Empty == Empty = True
  _     == _     = False

eqExtract :: (Ord k, Eq a) => k -> a -> BinomForest rk k a -> k -> a -> BinomForest rk k a -> Bool
eqExtract k10 a10 ts10 k20 a20 ts20 =
  k10 == k20 && a10 == a20 &&
  case (extract ts10, extract ts20) of
    (Yes (Extract k1 a1 _ ts1'), Yes (Extract k2 a2 _ ts2'))
             -> eqExtract k1 a1 ts1' k2 a2 ts2'
    (No, No) -> True
    _        -> False

instance (Ord k, Ord a) => Ord (MinPQueue k a) where
  MinPQ _n1 k10 a10 ts10 `compare` MinPQ _n2 k20 a20 ts20 =
    cmpExtract k10 a10 ts10 k20 a20 ts20
  Empty `compare` Empty   = EQ
  Empty `compare` MinPQ{} = LT
  MinPQ{} `compare` Empty = GT

cmpExtract :: (Ord k, Ord a) => k -> a -> BinomForest rk k a -> k -> a -> BinomForest rk k a -> Ordering
cmpExtract k10 a10 ts10 k20 a20 ts20 =
  k10 `compare` k20 <> a10 `compare` a20 <>
  case (extract ts10, extract ts20) of
    (Yes (Extract k1 a1 _ ts1'), Yes (Extract k2 a2 _ ts2'))
                -> cmpExtract k1 a1 ts1' k2 a2 ts2'
    (No, Yes{}) -> LT
    (Yes{}, No) -> GT
    (No, No)    -> EQ

-- | /O(1)/. Returns the empty priority queue.
empty :: MinPQueue k a
empty = Empty

-- | /O(1)/. Checks if this priority queue is empty.
null :: MinPQueue k a -> Bool
null Empty = True
null _     = False

-- | /O(1)/. Returns the size of this priority queue.
size :: MinPQueue k a -> Int
size Empty           = 0
size (MinPQ n _ _ _) = n

-- | /O(1)/. Constructs a singleton priority queue.
singleton :: k -> a -> MinPQueue k a
singleton k a = MinPQ 1 k a Nil

-- | Amortized /O(1)/, worst-case /O(log n)/. Inserts
-- an element with the specified key into the queue.
insert :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insert = insert' (<=)

-- | /O(n)/ (an earlier implementation had /O(1)/ but was buggy).
-- Insert an element with the specified key into the priority queue,
-- putting it behind elements whose key compares equal to the
-- inserted one.
insertBehind :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insertBehind k v q =
  let (smaller, larger) = spanKey (<= k) q
  in  foldr (uncurry insert) (insert k v larger) smaller

spanKey :: Ord k => (k -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
spanKey p q = case minViewWithKey q of
  Just (t@(k, _), q') | p k ->
    let (kas, q'') = spanKey p q' in (t : kas, q'')
  _ -> ([], q)

-- | Internal helper method, using a specific comparator function.
insert' :: CompF k -> k -> a -> MinPQueue k a -> MinPQueue k a
insert' _ k a Empty = singleton k a
insert' le k a (MinPQ n k' a' ts)
  | k `le` k' = MinPQ (n + 1) k  a  (incrMin (tip k' a') ts)
  | otherwise = MinPQ (n + 1) k' a' (incr le (tip k  a ) ts)

-- | Amortized /O(log(min(n1, n2)))/, worst-case /O(log(max(n1, n2)))/. Returns the union
-- of the two specified queues.
union :: Ord k => MinPQueue k a -> MinPQueue k a -> MinPQueue k a
union = union' (<=)

-- | Takes the union of the two specified queues, using the given comparison function.
union' :: CompF k -> MinPQueue k a -> MinPQueue k a -> MinPQueue k a
union' le (MinPQ n1 k1 a1 ts1) (MinPQ n2 k2 a2 ts2)
  | k1 `le` k2 = MinPQ (n1 + n2) k1 a1 (insMerge k2 a2)
  | otherwise  = MinPQ (n1 + n2) k2 a2 (insMerge k1 a1)
  where  insMerge k a = carryForest le (tip k a) ts1 ts2
union' _ Empty q2 = q2
union' _ q1 Empty = q1

-- | /O(1)/. The minimal (key, element) in the queue, if the queue is nonempty.
getMin :: MinPQueue k a -> Maybe (k, a)
getMin (MinPQ _ k a _) = Just (k, a)
getMin _               = Nothing

-- | /O(1)/. Alter the value at the minimum key. If the queue is empty, does nothing.
adjustMinWithKey :: (k -> a -> a) -> MinPQueue k a -> MinPQueue k a
adjustMinWithKey _ Empty = Empty
adjustMinWithKey f (MinPQ n k a ts) = MinPQ n k (f k a) ts

-- | /O(log n)/. (Actually /O(1)/ if there's no deletion.) Update the value at the minimum key.
-- If the queue is empty, does nothing.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> MinPQueue k a -> MinPQueue k a
updateMinWithKey _ Empty = Empty
updateMinWithKey f (MinPQ n k a ts) = case f k a of
  Nothing  -> extractHeap (<=) n ts
  Just a'  -> MinPQ n k a' ts

-- | /O(log n)/. Retrieves the minimal (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
minViewWithKey :: Ord k => MinPQueue k a -> Maybe ((k, a), MinPQueue k a)
minViewWithKey Empty            = Nothing
minViewWithKey (MinPQ n k a ts) = Just ((k, a), extractHeap (<=) n ts)

-- | /O(n)/. Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> MinPQueue k a -> MinPQueue k b
mapWithKey f = runIdentity . traverseWithKeyU (Identity .: f)

-- | /O(n)/. @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when @f@ is strictly
-- monotonic. /The precondition is not checked./ This function has better performance than
-- 'mapKeys'.
mapKeysMonotonic :: (k -> k') -> MinPQueue k a -> MinPQueue k' a
mapKeysMonotonic _ Empty = Empty
mapKeysMonotonic f (MinPQ n k a ts) = MinPQ n (f k) a (mapKeysMonoF f (const Zero) ts)

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> MinPQueue k a -> MinPQueue k b
mapMaybeWithKey _ Empty            = Empty
mapMaybeWithKey f (MinPQ _ k a ts) = maybe id (insert k) (f k a) (mapMaybeF (<=) f (const Empty) ts)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> MinPQueue k a -> (MinPQueue k b, MinPQueue k c)
mapEitherWithKey _ Empty            = (Empty, Empty)
mapEitherWithKey f (MinPQ _ k a ts) = either (first' . insert k) (second' . insert k) (f k a)
  (mapEitherF (<=) f (const (Empty, Empty)) ts)

-- | /O(n log n)/. Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toAscList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKey _ z Empty = z
foldrWithKey f z (MinPQ _ k0 a0 ts0) = f k0 a0 (foldF ts0) where
  foldF ts = case extract ts of
    Yes (Extract k a _ ts') -> f k a (foldF ts')
    _                       -> z

-- | /O(n log n)/. Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toAscList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: Ord k => (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKey _ z Empty = z
foldlWithKey f z0 (MinPQ _ k0 a0 ts0) = foldF (f z0 k0 a0) ts0 where
  foldF z ts = case extract ts of
    Yes (Extract k a _ ts') -> foldF (f z k a) ts'
    _                       -> z

{-# INLINABLE [1] toAscList #-}
-- | /O(n log n)/. Return all (key, value) pairs in ascending order by key.
toAscList :: Ord k => MinPQueue k a -> [(k, a)]
toAscList = foldrWithKey (curry (:)) []

{-# INLINABLE [1] toDescList #-}
-- | /O(n log n)/. Return all (key, value) pairs in descending order by key.
toDescList :: Ord k => MinPQueue k a -> [(k, a)]
toDescList = foldlWithKey (\z k a -> (k, a) : z) []

-- | /O(n)/. Build a priority queue from an ascending list of (key, value) pairs. /The precondition is not checked./
fromAscList :: [(k, a)] -> MinPQueue k a
{-# INLINE fromAscList #-}
fromAscList xs = List.foldl' (\q (k, a) -> insertMax' k a q) empty xs

{-# RULES
  "toAscList" toAscList = \q -> build (\c n -> foldrWithKey (curry c) n q);
  "toDescList" toDescList = \q -> build (\c n -> foldlWithKey (\z k a -> (k, a) `c` z) n q);
  "toListU" toListU = \q -> build (\c n -> foldrWithKeyU (curry c) n q);
  #-}

{-# NOINLINE toListU #-}
-- | /O(n)/. Returns all (key, value) pairs in the queue in no particular order.
toListU :: MinPQueue k a -> [(k, a)]
toListU = foldrWithKeyU (curry (:)) []

-- | /O(n)/. An unordered right fold over the elements of the queue, in no particular order.
foldrU :: (a -> b -> b) -> b -> MinPQueue k a -> b
foldrU = foldrWithKeyU . const

-- | Equivalent to 'insert', save the assumption that this key is @<=@
-- every other key in the map. /The precondition is not checked./
insertMin :: k -> a -> MinPQueue k a -> MinPQueue k a
insertMin k a Empty = MinPQ 1 k a Nil
insertMin k a (MinPQ n k' a' ts) = MinPQ (n + 1) k a (incrMin (tip k' a') ts)

-- | Equivalent to 'insert', save the assumption that this key is @<=@
-- every other key in the map. /The precondition is not checked./ Additionally,
-- this eagerly constructs the new portion of the spine.
insertMin' :: k -> a -> MinPQueue k a -> MinPQueue k a
insertMin' k a Empty = MinPQ 1 k a Nil
insertMin' k a (MinPQ n k' a' ts) = MinPQ (n + 1) k a (incrMin' (tip k' a') ts)

-- | Inserts an entry with key @>=@ every key in the map. Assumes and preserves
-- an extra invariant: the roots of the binomial trees are decreasing along
-- the spine.
insertMax' :: k -> a -> MinPQueue k a -> MinPQueue k a
insertMax' k a Empty = MinPQ 1 k a Nil
insertMax' k a (MinPQ n k' a' ts) = MinPQ (n + 1) k' a' (incrMax' (tip k a) ts)

{-# INLINE fromList #-}
-- | /O(n)/. Constructs a priority queue from an unordered list.
fromList :: Ord k => [(k, a)] -> MinPQueue k a
-- We build a forest first and then extract its minimum at the end.
-- Why not just build the 'MinQueue' directly? This way saves us one
-- comparison per element.
fromList xs = case extractForest (<=) (fromListHeap (<=) xs) of
  No -> Empty
  -- Should we track the size as we go instead? That saves O(log n)
  -- at the end, but it needs an extra register all along the way.
  -- The nodes should probably all be in L1 cache already thanks to the
  -- extractHeap.
  Yes (Extract k v ~Zero f) -> MinPQ (sizeHeap f + 1) k v f

{-# INLINE fromListHeap #-}
fromListHeap :: CompF k -> [(k, a)] -> BinomHeap k a
fromListHeap le xs = List.foldl' go Nil xs
  where
    go fr (k, a) = incr' le (tip k a) fr

sizeHeap :: BinomHeap k a -> Int
sizeHeap = go 0 1
  where
    go :: Int -> Int -> BinomForest rk k a -> Int
    go acc rk Nil = rk `seq` acc
    go acc rk (Skip f) = go acc (2 * rk) f
    go acc rk (Cons _t f) = go (acc + rk) (2 * rk) f

-- | /O(1)/. Returns a binomial tree of rank zero containing this
-- key and value.
tip :: k -> a -> BinomTree Zero k a
tip k a = BinomTree k a Zero

-- | /O(1)/. Takes the union of two binomial trees of the same rank.
meld :: CompF k -> BinomTree rk k a -> BinomTree rk k a -> BinomTree (Succ rk) k a
meld le t1@(BinomTree k1 v1 ts1) t2@(BinomTree k2 v2 ts2)
  | k1 `le` k2 = BinomTree k1 v1 (Succ t2 ts1)
  | otherwise  = BinomTree k2 v2 (Succ t1 ts2)

-- | Takes the union of two binomial forests, starting at the same rank. Analogous to binary addition.
mergeForest :: CompF k -> BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
mergeForest le f1 f2 = case (f1, f2) of
  (Skip ts1, Skip ts2)       -> Skip $! mergeForest le ts1 ts2
  (Skip ts1, Cons t2 ts2)    -> Cons t2 $! mergeForest le ts1 ts2
  (Cons t1 ts1, Skip ts2)    -> Cons t1 $! mergeForest le ts1 ts2
  (Cons t1 ts1, Cons t2 ts2) -> Skip $! carryForest le (meld le t1 t2) ts1 ts2
  (Nil, _)                   -> f2
  (_, Nil)                   -> f1

-- | Takes the union of two binomial forests, starting at the same rank, with an additional tree.
-- Analogous to binary addition when a digit has been carried.
carryForest :: CompF k -> BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
carryForest le t0 f1 f2 = t0 `seq` case (f1, f2) of
  (Cons t1 ts1, Cons t2 ts2) -> Cons t0 $! carryMeld t1 t2 ts1 ts2
  (Cons t1 ts1, Skip ts2)    -> Skip $! carryMeld t0 t1 ts1 ts2
  (Skip ts1, Cons t2 ts2)    -> Skip $! carryMeld t0 t2 ts1 ts2
  (Skip ts1, Skip ts2)       -> Cons t0 $! mergeForest le ts1 ts2
  -- Why do these use incr and not incr'? We want the merge to take
  -- O(log(min(|f1|, |f2|))) amortized time. If we performed this final
  -- increment eagerly, that would degrade to O(log(max(|f1|, |f2|))) time.
  (Nil, _)                   -> incr le t0 f2
  (_, Nil)                   -> incr le t0 f1
  where  carryMeld = carryForest le .: meld le

-- | Inserts a binomial tree into a binomial forest. Analogous to binary incrementation.
incr :: CompF k -> BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incr le t ts = t `seq` case ts of
  Nil         -> Cons t Nil
  Skip ts'    -> Cons t ts'
  Cons t' ts' -> ts' `seq` Skip (incr le (meld le t t') ts')

-- | Inserts a binomial tree into a binomial forest. Analogous to binary incrementation.
-- Forces the rebuilt portion of the spine.
incr' :: CompF k -> BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incr' le t ts = t `seq` case ts of
  Nil         -> Cons t Nil
  Skip ts'    -> Cons t ts'
  Cons t' ts' -> Skip $! incr' le (meld le t t') ts'

-- | Inserts a binomial tree into a binomial forest. Assumes that the root of this tree
-- is less than all other roots. Analogous to binary incrementation. Equivalent to
-- @'incr' (\_ _ -> True)@.
incrMin :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMin t@(BinomTree k a ts) tss = case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons t' tss' -> tss' `seq` Skip (incrMin (BinomTree k a (Succ t' ts)) tss')

-- | Inserts a binomial tree into a binomial forest. Assumes that the root of this tree
-- is less than all other roots. Analogous to binary incrementation. Equivalent to
-- @'incr'' (\_ _ -> True)@. Forces the rebuilt portion of the spine.
incrMin' :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMin' t@(BinomTree k a ts) tss = case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons t' tss' -> Skip $! incrMin' (BinomTree k a (Succ t' ts)) tss'

-- | See 'insertMax'' for invariant info.
incrMax' :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMax' t tss = t `seq` case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons (BinomTree k a ts) tss' -> Skip $! incrMax' (BinomTree k a (Succ t ts)) tss'

extractHeap :: CompF k -> Int -> BinomHeap k a -> MinPQueue k a
extractHeap le n ts = n `seq` case extractForest le ts of
  No                      -> Empty
  Yes (Extract k a _ ts') -> MinPQ (n - 1) k a ts'

-- | A specialized type intended to organize the return of extract-min queries
-- from a binomial forest. We walk all the way through the forest, and then
-- walk backwards. @Extract rk a@ is the result type of an extract-min
-- operation that has walked as far backwards of rank @rk@ -- that is, it
-- has visited every root of rank @>= rk@.
--
-- The interpretation of @Extract minKey minVal children forest@ is
--
--   * @minKey@ is the key of the minimum root visited so far. It may have
--     any rank @>= rk@. We will denote the root corresponding to
--     @minKey@ as @minRoot@.
--
--   * @minVal@ is the value corresponding to @minKey@.
--
--   * @children@ is those children of @minRoot@ which have not yet been
--     merged with the rest of the forest. Specifically, these are
--     the children with rank @< rk@.
--
--   * @forest@ is an accumulating parameter that maintains the partial
--     reconstruction of the binomial forest without @minRoot@. It is
--     the union of all old roots with rank @>= rk@ (except @minRoot@),
--     with the set of all children of @minRoot@ with rank @>= rk@.
--     Note that @forest@ is lazy, so if we discover a smaller key
--     than @minKey@ later, we haven't wasted significant work.

data Extract rk k a = Extract !k a !(rk k a) !(BinomForest rk k a)
data MExtract rk k a = No | Yes {-# UNPACK #-} !(Extract rk k a)

incrExtract :: Extract (Succ rk) k a -> Extract rk k a
incrExtract (Extract minKey minVal (Succ kChild kChildren) ts)
  = Extract minKey minVal kChildren (Cons kChild ts)

-- Why are we so lazy here? The idea, right or not, is to avoid a potentially
-- expensive second pass to propagate carries. Instead, carry propagation gets
-- fused (operationally) with successive operations. If the next operation is
-- union or minView, this doesn't save anything, but if some insertions follow,
-- it might be faster this way.
incrExtract' :: CompF k -> BinomTree rk k a -> Extract (Succ rk) k a -> Extract rk k a
incrExtract' le t (Extract minKey minVal (Succ kChild kChildren) ts)
  = Extract minKey minVal kChildren (Skip $ incr le (t `cat` kChild) ts)
  where
    cat = meld le

-- | Walks backward from the biggest key in the forest, as far as rank @rk@.
-- Returns its progress. Each successive application of @extractBin@ takes
-- amortized /O(1)/ time, so applying it from the beginning takes /O(log n)/ time.
extractForest :: CompF k -> BinomForest rk k a -> MExtract rk k a
extractForest le0 = start le0
  where
    start :: CompF k -> BinomForest rk k a -> MExtract rk k a
    start _le Nil = No
    start le (Skip f) = case start le f of
      No     -> No
      Yes ex -> Yes (incrExtract ex)
    start le (Cons t@(BinomTree k v ts) f) = Yes $ case go le k f of
      No -> Extract k v ts (Skip f)
      Yes ex -> incrExtract' le t ex

    go :: CompF k -> k -> BinomForest rk k a -> MExtract rk k a
    go _le _min_above Nil = _min_above `seq` No
    go le min_above (Skip f) = case go le min_above f of
      No -> No
      Yes ex -> Yes (incrExtract ex)
    go le min_above (Cons t@(BinomTree k v ts) f)
      | min_above `le` k = case go le min_above f of
          No -> No
          Yes ex -> Yes (incrExtract' le t ex)
      | otherwise = case go le k f of
          No -> Yes (Extract k v ts (Skip f))
          Yes ex -> Yes (incrExtract' le t ex)

extract :: (Ord k) => BinomForest rk k a -> MExtract rk k a
extract = extractForest (<=)

-- | Utility function for mapping over a forest.
mapForest :: (k -> a -> b) -> (rk k a -> rk k b) -> BinomForest rk k a -> BinomForest rk k b
mapForest f fCh ts0 = case ts0 of
  Nil      -> Nil
  Skip ts' -> Skip (mapForest f fCh' ts')
  Cons (BinomTree k a ts) tss
           -> Cons (BinomTree k (f k a) (fCh ts)) (mapForest f fCh' tss)
  where fCh' (Succ (BinomTree k a ts) tss)
           = Succ (BinomTree k (f k a) (fCh ts)) (fCh tss)

-- | Utility function for mapping a 'Maybe' function over a forest.
mapMaybeF :: CompF k -> (k -> a -> Maybe b) -> (rk k a -> MinPQueue k b) ->
  BinomForest rk k a -> MinPQueue k b
mapMaybeF le f fCh ts0 = case ts0 of
  Nil    -> Empty
  Skip ts'  -> mapMaybeF le f fCh' ts'
  Cons (BinomTree k a ts) ts'
      -> insF k a (fCh ts) (mapMaybeF le f fCh' ts')
  where  insF k a = maybe id (insert' le k) (f k a) .: union' le
         fCh' (Succ (BinomTree k a ts) tss) =
           insF k a (fCh ts) (fCh tss)

-- | Utility function for mapping an 'Either' function over a forest.
mapEitherF :: CompF k -> (k -> a -> Either b c) -> (rk k a -> (MinPQueue k b, MinPQueue k c)) ->
  BinomForest rk k a -> (MinPQueue k b, MinPQueue k c)
mapEitherF le f0 fCh ts0 = case ts0 of
  Nil    -> (Empty, Empty)
  Skip ts'  -> mapEitherF le f0 fCh' ts'
  Cons (BinomTree k a ts) ts'
      -> insF k a (fCh ts) (mapEitherF le f0 fCh' ts')
  where
    insF k a = either (first' . insert' le k) (second' . insert' le k) (f0 k a) .:
      (union' le `both` union' le)
    fCh' (Succ (BinomTree k a ts) tss) =
      insF k a (fCh ts) (fCh tss)
    both f g (x1, x2) (y1, y2) = (f x1 y1, g x2 y2)

-- | /O(n)/. An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKeyU _ z Empty            = z
foldrWithKeyU f z (MinPQ _ k a ts) = f k a (foldrWithKeyF_ f (const id) ts z)

-- | /O(n)/. An unordered monoidal fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldMapWithKeyU :: Monoid m => (k -> a -> m) -> MinPQueue k a -> m
foldMapWithKeyU _ Empty            = mempty
foldMapWithKeyU f (MinPQ _ k a ts) = f k a `mappend` foldMapWithKey_ f ts

-- | /O(n)/. An unordered left fold over the elements of the queue, in no
-- particular order. This is rarely what you want; 'foldrWithKeyU' and
-- 'foldlWithKeyU'' are more likely to perform well.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKeyU _ z Empty = z
foldlWithKeyU f z0 (MinPQ _ k0 a0 ts) = foldlWithKeyF_ (\k a z -> f z k a) (const id) ts (f z0 k0 a0)

-- | /O(n)/. An unordered strict left fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldlWithKeyU' :: (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKeyU' _ z Empty = z
foldlWithKeyU' f !z0 (MinPQ _ k0 a0 ts) = foldlWithKey'_ f (f z0 k0 a0) ts

-- | /O(n)/. Map a function over all values in the queue.
map :: (a -> b) -> MinPQueue k a -> MinPQueue k b
map = mapWithKey . const

-- | /O(n log n)/. Traverses the elements of the queue in ascending order by key.
-- (@'traverseWithKey' f q == 'fromAscList' <$> 'traverse' ('uncurry' f) ('toAscList' q)@)
--
-- If you do not care about the /order/ of the traversal, consider using 'traverseWithKeyU'.
--
-- If you are working in a strict monad, consider using 'mapMWithKey'.
traverseWithKey :: (Ord k, Applicative f) => (k -> a -> f b) -> MinPQueue k a -> f (MinPQueue k b)
traverseWithKey f q = case minViewWithKey q of
  Nothing      -> pure empty
  Just ((k, a), q')  -> liftA2 (insertMin k) (f k a) (traverseWithKey f q')

-- | A strictly accumulating version of 'traverseWithKey'. This works well in
-- 'IO' and strict @State@, and is likely what you want for other "strict" monads,
-- where @⊥ >>= pure () = ⊥@.
mapMWithKey :: (Ord k, Monad m) => (k -> a -> m b) -> MinPQueue k a -> m (MinPQueue k b)
mapMWithKey f = go empty
  where
    go !acc q =
      case minViewWithKey q of
        Nothing           -> pure acc
        Just ((k, a), q') -> do
          b <- f k a
          let !acc' = insertMax' k b acc
          go acc' q'

-- | /O(n)/. An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
traverseWithKeyU :: Applicative f => (k -> a -> f b) -> MinPQueue k a -> f (MinPQueue k b)
traverseWithKeyU _ Empty = pure Empty
traverseWithKeyU f (MinPQ n k a ts) = liftA2 (MinPQ n k) (f k a) (traverseForest f (const (pure Zero)) ts)

{-# SPECIALIZE traverseForest :: (k -> a -> Identity b) -> (rk k a -> Identity (rk k b)) -> BinomForest rk k a ->
  Identity (BinomForest rk k b) #-}
traverseForest :: (Applicative f) => (k -> a -> f b) -> (rk k a -> f (rk k b)) -> BinomForest rk k a -> f (BinomForest rk k b)
traverseForest f fCh ts0 = case ts0 of
  Nil       -> pure Nil
  Skip ts'  -> Skip <$> traverseForest f fCh' ts'
  Cons (BinomTree k a ts) tss
    -> liftA3 (\p q -> Cons (BinomTree k p q)) (f k a) (fCh ts) (traverseForest f fCh' tss)
  where
    fCh' (Succ (BinomTree k a ts) tss)
      = Succ <$> (BinomTree k <$> f k a <*> fCh ts) <*> fCh tss

-- | Unordered right fold on a binomial forest.
foldrWithKeyF_ :: (k -> a -> b -> b) -> (rk k a -> b -> b) -> BinomForest rk k a -> b -> b
foldrWithKeyF_ f fCh ts0 z0 = case ts0 of
  Nil    -> z0
  Skip ts'  -> foldrWithKeyF_ f fCh' ts' z0
  Cons (BinomTree k a ts) ts'
    -> f k a (fCh ts (foldrWithKeyF_ f fCh' ts' z0))
  where
    fCh' (Succ (BinomTree k a ts) tss) z =
      f k a (fCh ts (fCh tss z))

-- | Unordered left fold on a binomial forest.
foldlWithKeyF_ :: (k -> a -> b -> b) -> (rk k a -> b -> b) -> BinomForest rk k a -> b -> b
foldlWithKeyF_ f fCh ts0 = case ts0 of
  Nil    -> id
  Skip ts'  -> foldlWithKeyF_ f fCh' ts'
  Cons (BinomTree k a ts) ts'
    -> foldlWithKeyF_ f fCh' ts' . fCh ts . f k a
  where
    fCh' (Succ (BinomTree k a ts) tss) =
      fCh tss . fCh ts . f k a

-- | Maps a monotonic function over the keys in a binomial forest.
mapKeysMonoF :: (k -> k') -> (rk k a -> rk k' a) -> BinomForest rk k a -> BinomForest rk k' a
mapKeysMonoF f fCh ts0 = case ts0 of
  Nil    -> Nil
  Skip ts'  -> Skip (mapKeysMonoF f fCh' ts')
  Cons (BinomTree k a ts) ts'
    -> Cons (BinomTree (f k) a (fCh ts)) (mapKeysMonoF f fCh' ts')
  where
    fCh' (Succ (BinomTree k a ts) tss) =
      Succ (BinomTree (f k) a (fCh ts)) (fCh tss)

-- | /O(log n)/. @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MinPQueue' is stored somewhat lazily. Most operations
-- take great care to prevent chains of thunks from accumulating along the
-- spine to the detriment of performance. However, 'mapKeysMonotonic' can leave
-- expensive thunks in the structure and repeated applications of that function
-- can create thunk chains.
seqSpine :: MinPQueue k a -> b -> b
seqSpine Empty z0 = z0
seqSpine (MinPQ _ _ _ ts0) z0 = ts0 `seqSpineF` z0 where
  seqSpineF :: BinomForest rk k a -> b -> b
  seqSpineF ts z = case ts of
    Nil        -> z
    Skip ts'   -> seqSpineF ts' z
    Cons _ ts' -> seqSpineF ts' z

class NFRank rk where
  rnfRk :: (NFData k, NFData a) => rk k a -> ()

instance NFRank Zero where
  rnfRk _ = ()

instance NFRank rk => NFRank (Succ rk) where
  rnfRk (Succ t ts) = t `deepseq` rnfRk ts

instance (NFData k, NFData a, NFRank rk) => NFData (BinomTree rk k a) where
  rnf (BinomTree k a ts) = k `deepseq` a `deepseq` rnfRk ts

instance (NFData k, NFData a, NFRank rk) => NFData (BinomForest rk k a) where
  rnf Nil = ()
  rnf (Skip tss) = rnf tss
  rnf (Cons t tss) = t `deepseq` rnf tss

instance (NFData k, NFData a) => NFData (MinPQueue k a) where
  rnf Empty = ()
  rnf (MinPQ _ k a ts) = k `deepseq` a `deepseq` rnf ts

instance Functor (MinPQueue k) where
  fmap = map

instance Ord k => Foldable (MinPQueue k) where
  foldr   = foldrWithKey . const
  foldl f = foldlWithKey (const . f)
  length = size
  null = null

-- | Traverses in ascending order. 'mapM' is strictly accumulating like
-- 'mapMWithKey'.
instance Ord k => Traversable (MinPQueue k) where
  traverse = traverseWithKey . const
  mapM = mapMWithKey . const
  sequence = mapM id
