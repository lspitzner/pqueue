{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PQueue.Prio.Internals (
  MinPQueue(..),
  BinomForest(..),
  BinomHeap,
  BinomTree(..),
  Zero(..),
  Succ(..),
  empty,
  null,
  size,
  singleton,
  insert,
  insertBehind,
  insertEager,
  union,
  getMin,
  adjustMinWithKey,
  adjustMinWithKeyA',
  updateMinWithKey,
  updateMinWithKeyA',
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
  unions
  ) where

#if MIN_VERSION_base(4,18,0)
import Control.Applicative (Const (..))
#else
import Control.Applicative (liftA2, Const (..))
#endif
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import qualified Data.List as List

import Data.Semigroup (Semigroup(..), stimesMonoid, Endo (..), Dual (..))

import Prelude hiding (null, map)
#ifdef __GLASGOW_HASKELL__
import Data.Data
import GHC.Exts (build, inline)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
  readPrec, readListPrec, readListPrecDefault)
#endif

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
import Nattish (Nattish (..))

#ifndef __GLASGOW_HASKELL__
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

#if __GLASGOW_HASKELL__

-- | Treats the priority queue as an empty queue or a minimal
-- key-value pair and a priority queue. The constructors, conceptually,
-- are 'Data.PQueue.Prio.Min.Empty' and '(Data.PQueue.Prio.Min.:<)'.
--
-- 'gfoldl' is nondeterministic; any minimal pair may be chosen as
-- the first. All constructed queues maintain the queue invariants.
instance (Ord k, Data k, Data a) => Data (MinPQueue k a) where
  gfoldl f z q = case minViewWithKey q of
    Nothing      -> z Empty
    Just (x, q') -> z (\(k, a) -> insert k a) `f` x `f` q'

  gunfold k z c = case constrIndex c of
    1 -> z Empty
    2 -> k (k (z (\(key, val) -> insert key val)))
    _ -> error "gunfold: invalid constructor for MinPQueue"

  toConstr q
    | null q = emptyConstr
    | otherwise = consConstr

  dataTypeOf _ = queueDataType
  dataCast1 f  = gcast1 f
  dataCast2 f  = gcast2 f

queueDataType :: DataType
queueDataType = mkDataType "Data.PQueue.Prio.Min.MinPQueue" [emptyConstr, consConstr]

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr queueDataType "Empty" [] Prefix
consConstr  = mkConstr queueDataType ":<" [] Infix
#endif

instance Ord k => Semigroup (MinPQueue k a) where
  (<>) = union
  stimes = stimesMonoid
  {-# INLINABLE stimes #-}

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

infixr 8 .:

-- | A priority queue where keys of type @k@ are annotated with values of type
-- @a@.  The queue supports extracting the key-value pair with minimum key.
data MinPQueue k a = Empty | MinPQ {-# UNPACK #-} !Int !k a !(BinomHeap k a)

data BinomForest rk k a =
  Nil |
  Skip (BinomForest (Succ rk) k a) |
  Cons {-# UNPACK #-} !(BinomTree rk k a) (BinomForest (Succ rk) k a)
type BinomHeap = BinomForest Zero

data BinomTree rk k a = BinomTree !k (rk k a)
newtype Zero k a = Zero a
data Succ rk k a = Succ {-# UNPACK #-} !(BinomTree rk k a) (rk k a)

instance (Ord k, Eq a) => Eq (MinPQueue k a) where
  MinPQ n1 k1 a1 ts1 == MinPQ n2 k2 a2 ts2 =
    n1 == n2 && eqExtract k1 a1 ts1 k2 a2 ts2
  Empty == Empty = True
  _     == _     = False

eqExtract :: (Ord k, Eq a) => k -> a -> BinomHeap k a -> k -> a -> BinomHeap k a -> Bool
eqExtract k10 a10 ts10 k20 a20 ts20 =
  k10 == k20 && a10 == a20 &&
  case (extract ts10, extract ts20) of
    (Yes (Extract k1 (Zero a1) ts1'), Yes (Extract k2 (Zero a2) ts2'))
             -> eqExtract k1 a1 ts1' k2 a2 ts2'
    (No, No) -> True
    _        -> False

instance (Ord k, Ord a) => Ord (MinPQueue k a) where
  MinPQ _n1 k10 a10 ts10 `compare` MinPQ _n2 k20 a20 ts20 =
    cmpExtract k10 a10 ts10 k20 a20 ts20
  Empty `compare` Empty   = EQ
  Empty `compare` MinPQ{} = LT
  MinPQ{} `compare` Empty = GT

cmpExtract :: (Ord k, Ord a) => k -> a -> BinomHeap k a -> k -> a -> BinomHeap k a -> Ordering
cmpExtract k10 a10 ts10 k20 a20 ts20 =
  k10 `compare` k20 <> a10 `compare` a20 <>
  case (extract ts10, extract ts20) of
    (Yes (Extract k1 (Zero a1) ts1'), Yes (Extract k2 (Zero a2) ts2'))
                -> cmpExtract k1 a1 ts1' k2 a2 ts2'
    (No, Yes{}) -> LT
    (Yes{}, No) -> GT
    (No, No)    -> EQ

-- | \(O(1)\). Returns the empty priority queue.
empty :: MinPQueue k a
empty = Empty

-- | \(O(1)\). Checks if this priority queue is empty.
null :: MinPQueue k a -> Bool
null Empty = True
null _     = False

-- | \(O(1)\). Returns the size of this priority queue.
size :: MinPQueue k a -> Int
size Empty           = 0
size (MinPQ n _ _ _) = n

-- | \(O(1)\). Constructs a singleton priority queue.
singleton :: k -> a -> MinPQueue k a
singleton k a = MinPQ 1 k a Nil

-- | Amortized \(O(1)\), worst-case \(O(\log n)\). Inserts
-- an element with the specified key into the queue.
insert :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insert k a Empty = singleton k a
insert k a (MinPQ n k' a' ts)
  | k <= k' = MinPQ (n + 1) k  a  (incrMin (tip k' a') ts)
  | otherwise = MinPQ (n + 1) k' a' (incr (tip k  a ) ts)

insertEager :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insertEager k a Empty = singleton k a
insertEager k a (MinPQ n k' a' ts)
  | k <= k' = MinPQ (n + 1) k a  (insertEagerHeap k' a' ts)
  | otherwise = MinPQ (n + 1) k' a' (insertEagerHeap k a ts)

-- | \(O(n)\) (an earlier implementation had \(O(1)\) but was buggy).
-- Insert an element with the specified key into the priority queue,
-- putting it behind elements whose key compares equal to the
-- inserted one.
{-# DEPRECATED insertBehind "This function is not reliable." #-}
insertBehind :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insertBehind k v q =
  let (smaller, larger) = spanKey (<= k) q
  in  foldr (uncurry insert) (insert k v larger) smaller

spanKey :: Ord k => (k -> Bool) -> MinPQueue k a -> ([(k, a)], MinPQueue k a)
spanKey p q = case minViewWithKey q of
  Just (t@(k, _), q') | p k ->
    let (kas, q'') = spanKey p q' in (t : kas, q'')
  _ -> ([], q)

-- | Amortized \(O(\log \min(n_1,n_2))\), worst-case \(O(\log \max(n_1,n_2))\). Returns the union
-- of the two specified queues.
union :: Ord k => MinPQueue k a -> MinPQueue k a -> MinPQueue k a
union (MinPQ n1 k1 a1 ts1) (MinPQ n2 k2 a2 ts2)
  | k1 <= k2 = MinPQ (n1 + n2) k1 a1 (insMerge k2 a2)
  | otherwise  = MinPQ (n1 + n2) k2 a2 (insMerge k1 a1)
  where  insMerge k a = carryForest (tip k a) ts1 ts2
union Empty q2 = q2
union q1 Empty = q1

-- | \(O(1)\). The minimal (key, element) in the queue, if the queue is nonempty.
getMin :: MinPQueue k a -> Maybe (k, a)
getMin (MinPQ _ k a _) = Just (k, a)
getMin _               = Nothing

-- | \(O(1)\). Alter the value at the minimum key. If the queue is empty, does nothing.
adjustMinWithKey :: (k -> a -> a) -> MinPQueue k a -> MinPQueue k a
adjustMinWithKey _ Empty = Empty
adjustMinWithKey f (MinPQ n k a ts) = MinPQ n k (f k a) ts

-- | \(O(1)\) per operation. Alter the value at the minimum key in an 'Applicative' context. If the
-- queue is empty, does nothing.
adjustMinWithKeyA' :: Applicative f => (MinPQueue k a -> r) -> (k -> a -> f a) -> MinPQueue k a -> f r
adjustMinWithKeyA' g _ Empty = pure (g Empty)
adjustMinWithKeyA' g f (MinPQ n k a ts) = fmap (\a' -> g (MinPQ n k a' ts)) (f k a)

-- | \(O(\log n)\). (Actually \(O(1)\) if there's no deletion.) Update the value at the minimum key.
-- If the queue is empty, does nothing.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> MinPQueue k a -> MinPQueue k a
updateMinWithKey _ Empty = Empty
updateMinWithKey f (MinPQ n k a ts) = case f k a of
  Nothing  -> extractHeap n ts
  Just a'  -> MinPQ n k a' ts

-- | \(O(\log n)\) per operation. (Actually \(O(1)\) if there's no deletion.) Update
-- the value at the minimum key in an 'Applicative' context. If the queue is
-- empty, does nothing.
updateMinWithKeyA'
  :: (Applicative f, Ord k)
  => (MinPQueue k a -> r)
  -> (k -> a -> f (Maybe a))
  -> MinPQueue k a
  -> f r
updateMinWithKeyA' g _ Empty = pure (g Empty)
updateMinWithKeyA' g f (MinPQ n k a ts) = fmap (g . tweak) (f k a)
  where
    tweak Nothing = extractHeap n ts
    tweak (Just a') = MinPQ n k a' ts

-- | \(O(\log n)\). Retrieves the minimal (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
minViewWithKey :: Ord k => MinPQueue k a -> Maybe ((k, a), MinPQueue k a)
minViewWithKey Empty            = Nothing
minViewWithKey (MinPQ n k a ts) = Just ((k, a), extractHeap n ts)

-- | \(O(n)\). Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> MinPQueue k a -> MinPQueue k b
mapWithKey f = runIdentity . traverseWithKeyU (coerce f)

-- | \(O(n)\). @'mapKeysMonotonic' f q == 'Data.PQueue.Prio.Min.mapKeys' f q@,
-- but only works when @f@ is (weakly) monotonic (meaning that @x <= y@ implies
-- @f x <= f y@). /The precondition is not checked./ This function has better
-- performance than 'Data.PQueue.Prio.Min.mapKeys'.
--
-- Note: if the given function returns bottom for any of the keys in the queue, then the
-- portion of the queue which is bottom is /unspecified/.
mapKeysMonotonic :: (k -> k') -> MinPQueue k a -> MinPQueue k' a
mapKeysMonotonic _ Empty = Empty
mapKeysMonotonic f (MinPQ n k a ts) = MinPQ n (f k) a $! mapKeysMonoHeap f ts

mapKeysMonoHeap :: forall k k' a. (k -> k') -> BinomHeap k a -> BinomHeap k' a
mapKeysMonoHeap f = mapKeysMonoForest Zeroy
  where
    mapKeysMonoForest :: Ranky rk -> BinomForest rk k a -> BinomForest rk k' a
    mapKeysMonoForest !_rky Nil = Nil
    mapKeysMonoForest !rky (Skip rest) = Skip $! mapKeysMonoForest (Succy rky) rest
    mapKeysMonoForest !rky (Cons t rest) = Cons (mapKeysMonoTree rky t) $! mapKeysMonoForest (Succy rky) rest

    {-# INLINE mapKeysMonoTree #-}
    mapKeysMonoTree :: Ranky rk -> BinomTree rk k a -> BinomTree rk k' a
    mapKeysMonoTree Zeroy (BinomTree k (Zero a)) =
      -- We've reached a value, which we must not force.
      BinomTree (f k) (Zero a)
      -- We're not at a value; we force the result.
    mapKeysMonoTree (Succy rky) (BinomTree k ts) = BinomTree (f k) $! mapKeysMonoTrees rky ts

    mapKeysMonoTrees :: Ranky rk -> Succ rk k a -> Succ rk k' a
    mapKeysMonoTrees Zeroy (Succ t (Zero a)) =
      -- Don't force the value!
      Succ (mapKeysMonoTree Zeroy t) (Zero a)
    mapKeysMonoTrees (Succy rky) (Succ t ts) =
      -- Whew, no values; force the trees.
      Succ (mapKeysMonoTree (Succy rky) t) $! mapKeysMonoTrees rky ts

-- | \(O(n)\). Map values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> MinPQueue k a -> MinPQueue k b
mapMaybeWithKey f = fromBare .
  foldlWithKeyU'
    (\q k a -> case f k a of
        Nothing -> q
        Just b -> insertEagerHeap k b q)
    Nil
{-# INLINABLE mapMaybeWithKey #-}

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> MinPQueue k a -> (MinPQueue k b, MinPQueue k c)
mapEitherWithKey f q
  | (l, r) <- mapEitherHeap f q
  , let
      !l' = fromBare l
      !r' = fromBare r
  = (l', r')
{-# INLINABLE mapEitherWithKey #-}

data Partition k a b = Partition !(BinomHeap k a) !(BinomHeap k b)

fromPartition :: Partition k a b -> (BinomHeap k a, BinomHeap k b)
fromPartition (Partition p q) = (p, q)

mapEitherHeap :: Ord k => (k -> a -> Either b c) -> MinPQueue k a -> (BinomHeap k b, BinomHeap k c)
mapEitherHeap f = fromPartition .
  foldlWithKeyU'
    (\(Partition ls rs) k a ->
         case f k a of
           Left b -> Partition (insertEagerHeap k b ls) rs
           Right b -> Partition ls (insertEagerHeap k b rs))
    (Partition Nil Nil)

insertEagerHeap :: Ord k => k -> a -> BinomHeap k a -> BinomHeap k a
insertEagerHeap k a h = incr' (tip k a) h
{-# INLINE insertEagerHeap #-}

-- | \(O(n \log n)\). Fold the keys and values in the map, such that
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toAscList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKey _ z Empty = z
foldrWithKey f z (MinPQ _ k0 a0 ts0) = f k0 a0 (foldF ts0) where
  foldF ts = case extract ts of
    Yes (Extract k (Zero a) ts') -> f k a (foldF ts')
    No                           -> z

-- | \(O(n \log n)\). Fold the keys and values in the map, such that
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toAscList' q)@.
--
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: Ord k => (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKey _ z Empty = z
foldlWithKey f z0 (MinPQ _ k0 a0 ts0) = foldF (f z0 k0 a0) ts0 where
  foldF z ts = case extract ts of
    Yes (Extract k (Zero a) ts') -> foldF (f z k a) ts'
    No                           -> z

{-# INLINABLE [1] toAscList #-}
-- | \(O(n \log n)\). Return all (key, value) pairs in ascending order by key.
toAscList :: Ord k => MinPQueue k a -> [(k, a)]
toAscList = foldrWithKey (curry (:)) []

{-# INLINABLE [1] toDescList #-}
-- | \(O(n \log n)\). Return all (key, value) pairs in descending order by key.
toDescList :: Ord k => MinPQueue k a -> [(k, a)]
toDescList = foldlWithKey (\z k a -> (k, a) : z) []

-- | \(O(n)\). Build a priority queue from an ascending list of (key, value) pairs. /The precondition is not checked./
fromAscList :: [(k, a)] -> MinPQueue k a
{-# INLINE fromAscList #-}
fromAscList xs = List.foldl' (\q (k, a) -> insertMax' k a q) empty xs

{-# RULES
  "toAscList" toAscList = \q -> build (\c n -> foldrWithKey (curry c) n q);
  "toDescList" toDescList = \q -> build (\c n -> foldlWithKey (\z k a -> (k, a) `c` z) n q);
  "toListU" toListU = \q -> build (\c n -> foldrWithKeyU (curry c) n q);
  #-}

{-# NOINLINE toListU #-}
-- | \(O(n)\). Returns all (key, value) pairs in the queue in no particular order.
toListU :: MinPQueue k a -> [(k, a)]
toListU = foldrWithKeyU (curry (:)) []

-- | \(O(n)\). An unordered right fold over the elements of the queue, in no particular order.
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
-- | \(O(n)\). Constructs a priority queue from an unordered list.
fromList :: Ord k => [(k, a)] -> MinPQueue k a
-- We build a forest first and then extract its minimum at the end.  Why not
-- just build the 'MinQueue' directly? This way typically saves us one
-- comparison per element, which roughly halves comparisons.
fromList xs = fromBare (fromListHeap xs)

fromBare :: Ord k => BinomHeap k a -> MinPQueue k a
fromBare xs = case extract xs of
  No -> Empty
  -- Should we track the size as we go instead? That saves O(log n)
  -- at the end, but it needs an extra register all along the way.
  -- The nodes should probably all be in L1 cache already thanks to the
  -- extractHeap.
  Yes (Extract k (Zero v) f) -> MinPQ (sizeHeap f + 1) k v f

{-# INLINE fromListHeap #-}
fromListHeap :: Ord k => [(k, a)] -> BinomHeap k a
fromListHeap xs = List.foldl' go Nil xs
  where
    go fr (k, a) = insertEagerHeap k a fr

sizeHeap :: BinomHeap k a -> Int
sizeHeap = go 0 1
  where
    go :: Int -> Int -> BinomForest rk k a -> Int
    go acc rk Nil = rk `seq` acc
    go acc rk (Skip f) = go acc (2 * rk) f
    go acc rk (Cons _t f) = go (acc + rk) (2 * rk) f

-- | \(O(1)\). Returns a binomial tree of rank zero containing this
-- key and value.
tip :: k -> a -> BinomTree Zero k a
tip k a = BinomTree k (Zero a)

-- | \(O(1)\). Takes the union of two binomial trees of the same rank.
meld :: Ord k => BinomTree rk k a -> BinomTree rk k a -> BinomTree (Succ rk) k a
meld t1@(BinomTree k1 ts1) t2@(BinomTree k2 ts2)
  | k1 <= k2 = BinomTree k1 (Succ t2 ts1)
  | otherwise  = BinomTree k2 (Succ t1 ts2)

-- | Takes the union of two binomial forests, starting at the same rank. Analogous to binary addition.
mergeForest :: Ord k => BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
mergeForest f1 f2 = case (f1, f2) of
  (Skip ts1, Skip ts2)       -> Skip $! mergeForest ts1 ts2
  (Skip ts1, Cons t2 ts2)    -> Cons t2 $! mergeForest ts1 ts2
  (Cons t1 ts1, Skip ts2)    -> Cons t1 $! mergeForest ts1 ts2
  (Cons t1 ts1, Cons t2 ts2) -> Skip $! carryForest (meld t1 t2) ts1 ts2
  (Nil, _)                   -> f2
  (_, Nil)                   -> f1

-- | Takes the union of two binomial forests, starting at the same rank, with an additional tree.
-- Analogous to binary addition when a digit has been carried.
carryForest :: Ord k => BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
carryForest t0 f1 f2 = t0 `seq` case (f1, f2) of
  (Cons t1 ts1, Cons t2 ts2) -> Cons t0 $! carryMeld t1 t2 ts1 ts2
  (Cons t1 ts1, Skip ts2)    -> Skip $! carryMeld t0 t1 ts1 ts2
  (Skip ts1, Cons t2 ts2)    -> Skip $! carryMeld t0 t2 ts1 ts2
  (Skip ts1, Skip ts2)       -> Cons t0 $! mergeForest ts1 ts2
  -- Why do these use incr and not incr'? We want the merge to take
  -- O(log(min(|f1|, |f2|))) amortized time. If we performed this final
  -- increment eagerly, that would degrade to O(log(max(|f1|, |f2|))) time.
  (Nil, _)                   -> incr t0 f2
  (_, Nil)                   -> incr t0 f1
  where  carryMeld = carryForest .: meld

-- | Inserts a binomial tree into a binomial forest. Analogous to binary incrementation.
incr :: Ord k => BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incr t ts = t `seq` case ts of
  Nil         -> Cons t Nil
  Skip ts'    -> Cons t ts'
  Cons t' ts' -> ts' `seq` Skip (incr (meld t t') ts')

-- | Inserts a binomial tree into a binomial forest. Analogous to binary incrementation.
-- Forces the rebuilt portion of the spine.
incr' :: Ord k => BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incr' t ts = t `seq` case ts of
  Nil         -> Cons t Nil
  Skip ts'    -> Cons t ts'
  Cons t' ts' -> Skip $! incr' (meld t t') ts'

-- | Inserts a binomial tree into a binomial forest. Assumes that the root of this tree
-- is less than all other roots. Analogous to binary incrementation. Equivalent to
-- @'incr' (\_ _ -> True)@.
incrMin :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMin t@(BinomTree k ts) tss = case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons t' tss' -> tss' `seq` Skip (incrMin (BinomTree k (Succ t' ts)) tss')

-- | Inserts a binomial tree into a binomial forest. Assumes that the root of this tree
-- is less than all other roots. Analogous to binary incrementation. Equivalent to
-- @'incr'' (\_ _ -> True)@. Forces the rebuilt portion of the spine.
incrMin' :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMin' t@(BinomTree k ts) tss = case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons t' tss' -> Skip $! incrMin' (BinomTree k (Succ t' ts)) tss'

-- | See 'insertMax'' for invariant info.
incrMax' :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMax' t tss = t `seq` case tss of
  Nil          -> Cons t Nil
  Skip tss'    -> Cons t tss'
  Cons (BinomTree k ts) tss' -> Skip $! incrMax' (BinomTree k (Succ t ts)) tss'

extractHeap :: Ord k => Int -> BinomHeap k a -> MinPQueue k a
extractHeap n ts = n `seq` case extract ts of
  No                      -> Empty
  Yes (Extract k (Zero a) ts') -> MinPQ (n - 1) k a ts'

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

data Extract rk k a = Extract !k (rk k a) !(BinomForest rk k a)
data MExtract rk k a = No | Yes {-# UNPACK #-} !(Extract rk k a)

incrExtract :: Extract (Succ rk) k a -> Extract rk k a
incrExtract (Extract minKey (Succ kChild kChildren) ts)
  = Extract minKey kChildren (Cons kChild ts)

incrExtract' :: Ord k => BinomTree rk k a -> Extract (Succ rk) k a -> Extract rk k a
incrExtract' t (Extract minKey (Succ kChild kChildren) ts)
  = Extract minKey kChildren (Skip $! incr' (t `meld` kChild) ts)

-- | Walks backward from the biggest key in the forest, as far as rank @rk@.
-- Returns its progress. Each successive application of @extractBin@ takes
-- amortized \(O(1)\) time, so applying it from the beginning takes \(O(\log n)\) time.
extract :: Ord k => BinomForest rk k a -> MExtract rk k a
extract = start
  where
    start :: Ord k => BinomForest rk k a -> MExtract rk k a
    start Nil = No
    start (Skip f) = case start f of
      No     -> No
      Yes ex -> Yes (incrExtract ex)
    start (Cons t@(BinomTree k ts) f) = Yes $ case go k f of
      No -> Extract k ts (skip f)
      Yes ex -> incrExtract' t ex

    go :: Ord k => k -> BinomForest rk k a -> MExtract rk k a
    go _min_above Nil = _min_above `seq` No
    go min_above (Skip f) = case go min_above f of
      No -> No
      Yes ex -> Yes (incrExtract ex)
    go min_above (Cons t@(BinomTree k ts) f)
      | min_above <= k = case go min_above f of
          No -> No
          Yes ex -> Yes (incrExtract' t ex)
      | otherwise = case go k f of
          No -> Yes (Extract k ts (skip f))
          Yes ex -> Yes (incrExtract' t ex)

skip :: BinomForest (Succ rk) k a -> BinomForest rk k a
skip Nil = Nil
skip f = Skip f
{-# INLINE skip #-}

-- | \(O(n)\). An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKeyU c n = flip appEndo n . inline foldMapWithKeyU (coerce c)

-- | \(O(n)\). An unordered monoidal fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldMapWithKeyU :: forall m k a. Monoid m => (k -> a -> m) -> MinPQueue k a -> m
foldMapWithKeyU = coerce
  (inline traverseWithKeyU :: (k -> a -> Const m ()) -> MinPQueue k a -> Const m (MinPQueue k ()))

-- | \(O(n)\). An unordered left fold over the elements of the queue, in no
-- particular order. This is rarely what you want; 'foldrWithKeyU' and
-- 'foldlWithKeyU'' are more likely to perform well.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKeyU f b = flip appEndo b . getDual .
  foldMapWithKeyU (\k a -> Dual $ Endo $ \r -> f r k a)

-- | \(O(n)\). An unordered strict left fold over the elements of the queue, in no particular order.
--
-- @since 1.4.2
foldlWithKeyU' :: (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKeyU' f !b q =
  case q of
    Empty -> b
    MinPQ _n k a ts -> foldlHeapU' f (f b k a) ts

foldlHeapU' :: forall k a b. (b -> k -> a -> b) -> b -> BinomHeap k a -> b
foldlHeapU' f = \b -> foldlForest' Zeroy b
  where
    foldlForest' :: Ranky rk -> b -> BinomForest rk k a -> b
    foldlForest' !_rky !acc Nil = acc
    foldlForest' !rky !acc (Skip rest) = foldlForest' (Succy rky) acc rest
    foldlForest' !rky !acc (Cons t rest) =
      foldlForest' (Succy rky) (foldlTree' rky acc t) rest

    {-# INLINE foldlTree' #-}
    foldlTree' :: Ranky rk -> b -> BinomTree rk k a -> b
    foldlTree' !rky !acc (BinomTree k ts) = foldlTrees' rky acc k ts

    foldlTrees' :: Ranky rk -> b -> k -> rk k a -> b
    foldlTrees' Zeroy !acc !k (Zero a) = f acc k a
    foldlTrees' (Succy rky) !acc !k (Succ t ts) =
      foldlTrees' rky (foldlTree' rky acc t) k ts

-- | \(O(n \log n)\). Traverses the elements of the queue in ascending order by key.
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

-- | Natural numbers revealing whether something is 'Zero' or 'Succ'.
type Ranky = Nattish Zero Succ

-- | \(O(n)\). An unordered traversal over a priority queue, in no particular order.
-- While there is no guarantee in which order the elements are traversed, the resulting
-- priority queue will be perfectly valid.
{-# INLINABLE traverseWithKeyU #-}
traverseWithKeyU :: forall f k a b. Applicative f => (k -> a -> f b) -> MinPQueue k a -> f (MinPQueue k b)
traverseWithKeyU _ Empty = pure Empty
traverseWithKeyU f (MinPQ n k a ts) = liftA2 (\a' !ts' -> MinPQ n k a' ts') (f k a) (traverseHeapU f ts)

{-# INLINABLE traverseHeapU #-}
traverseHeapU :: forall f k a b. Applicative f => (k -> a -> f b) -> BinomHeap k a -> f (BinomHeap k b)
traverseHeapU f = traverseForest Zeroy
  where
    traverseForest :: Ranky rk -> BinomForest rk k a -> f (BinomForest rk k b)
    traverseForest !_rky Nil = pure Nil
    traverseForest !rky (Skip rest) = (Skip $!) <$> traverseForest (Succy rky) rest
    traverseForest !rky (Cons t rest) =
      liftA2 (\ !t' !rest' -> Cons t' rest') (traverseTree rky t) (traverseForest (Succy rky) rest)

    {-# INLINE traverseTree #-}
    traverseTree :: Ranky rk -> BinomTree rk k a -> f (BinomTree rk k b)
    traverseTree Zeroy (BinomTree k (Zero a)) =
      -- We've reached a value, so we don't force the result.
      BinomTree k . Zero <$> f k a
    traverseTree (Succy rky) (BinomTree k ts) =
      -- We're not at a value, so we force the tree list.
      (BinomTree k $!) <$> traverseTrees rky k ts

    traverseTrees :: Ranky rk -> k -> Succ rk k a -> f (Succ rk k b)
    traverseTrees Zeroy !k2 (Succ (BinomTree k1 (Zero a1)) (Zero a2)) =
      -- The right subtree is a value, so we don't force it.
      liftA2 (\b1 b2 -> Succ (BinomTree k1 (Zero b1)) (Zero b2)) (f k1 a1) (f k2 a2)
    traverseTrees (Succy rky) !k (Succ t ts) =
      -- Whew; no values. We're safe to force.
      liftA2 (\ !t' !ts' -> Succ t' ts') (traverseTree (Succy rky) t) (traverseTrees rky k ts)

-- | \(O(\log n)\). @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MinPQueue' is stored somewhat lazily. In earlier
-- versions of this package, some operations could produce chains of thunks
-- along the spine, occasionally necessitating manual forcing. Now, all
-- operations are careful to force enough to avoid this problem.
{-# DEPRECATED seqSpine "This function is no longer necessary or useful." #-}
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
  rnfRk (Zero a) = rnf a

instance NFRank rk => NFRank (Succ rk) where
  rnfRk (Succ t ts) = t `deepseq` rnfRk ts

instance (NFData k, NFData a, NFRank rk) => NFData (BinomTree rk k a) where
  rnf (BinomTree k ts) = k `deepseq` rnfRk ts

instance (NFData k, NFData a, NFRank rk) => NFData (BinomForest rk k a) where
  rnf Nil = ()
  rnf (Skip tss) = rnf tss
  rnf (Cons t tss) = t `deepseq` rnf tss

instance (NFData k, NFData a) => NFData (MinPQueue k a) where
  rnf Empty = ()
  rnf (MinPQ _ k a ts) = k `deepseq` a `deepseq` rnf ts

instance Functor (MinPQueue k) where
  fmap = imap . const

instance FunctorWithIndex k (MinPQueue k) where
  imap = coerce
    (traverseWithKeyU :: (k -> a -> Identity b) -> MinPQueue k a -> Identity (MinPQueue k b))

instance Ord k => Foldable (MinPQueue k) where
  foldr   = foldrWithKey . const
  foldl f = foldlWithKey (const . f)
  length = size
  null = null

instance Ord k => FoldableWithIndex k (MinPQueue k) where
  ifoldr   = foldrWithKey
  ifoldl f = foldlWithKey (flip f)

-- | Traverses in ascending order. 'mapM' is strictly accumulating like
-- 'mapMWithKey'.
instance Ord k => Traversable (MinPQueue k) where
  traverse = traverseWithKey . const
  mapM = mapMWithKey . const
  sequence = mapM id

instance Ord k => TraversableWithIndex k (MinPQueue k) where
  itraverse = traverseWithKey
