{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.PQueue.Internals (
  MinQueue (..),
  BinomHeap,
  BinomForest(..),
  BinomTree(..),
  Succ(..),
  Zero(..),
  empty,
  null,
  size,
  getMin,
  minView,
  singleton,
  insert,
  union,
  mapMaybe,
  mapEither,
  mapMonotonic,
  foldrAsc,
  foldlAsc,
  foldrDesc,
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
--   traverseU,
  seqSpine,
  unions
  ) where

import BinomialQueue.Internals
  ( BinomHeap
  , BinomForest (..)
  , BinomTree (..)
  , Succ (..)
  , Zero (..)
  , Extract (..)
  , MExtract (..)
  )
import qualified BinomialQueue.Internals as BQ
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Foldable (foldl')
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

-- | A priority queue with elements of type @a@. Supports extracting the minimum element.
data MinQueue a = Empty | MinQueue {-# UNPACK #-} !Int !a !(BQ.MinQueue a)

fromBare :: Ord a => BQ.MinQueue a -> MinQueue a
-- Should we fuse the size calculation with the minimum extraction?
fromBare xs = case BQ.minView xs of
  Just (x, xs') -> MinQueue (1 + BQ.size xs') x xs'
  Nothing -> Empty

#ifdef __GLASGOW_HASKELL__

-- | Treats the priority queue as an empty queue or a minimal element and a
-- priority queue. The constructors, conceptually, are 'Data.PQueue.Min.Empty'
-- and '(Data.PQueue.Min.:<)'. All constructed queues maintain the queue
-- invariants.
instance (Ord a, Data a) => Data (MinQueue a) where
  gfoldl f z q = case minView q of
    Nothing      -> z Empty
    Just (x, q') -> z insert `f` x `f` q'

  gunfold k z c = case constrIndex c of
    1 -> z Empty
    2 -> k (k (z insert))
    _ -> error "gunfold: invalid constructor for MinQueue"

  dataCast1 x = gcast1 x

  toConstr q
    | null q = emptyConstr
    | otherwise = consConstr

  dataTypeOf _ = queueDataType

queueDataType :: DataType
queueDataType = mkDataType "Data.PQueue.Min.MinQueue" [emptyConstr, consConstr]

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr queueDataType "Empty" [] Prefix
consConstr  = mkConstr queueDataType ":<" [] Infix

#endif

instance Ord a => Eq (MinQueue a) where
  Empty == Empty = True
  MinQueue n1 x1 q1 == MinQueue n2 x2 q2 =
    n1 == n2 && x1 == x2 && q1 == q2
  _ == _ = False

instance Ord a => Ord (MinQueue a) where
  Empty `compare` Empty = EQ
  Empty `compare` _ = LT
  _ `compare` Empty = GT
  MinQueue _n1 x1 q1 `compare` MinQueue _n2 x2 q2 = compare (x1,q1) (x2,q2)

    -- We compare their first elements, then their other elements up to the smaller queue's length,
    -- and then the longer queue wins.
    -- This is equivalent to @comparing toAscList@, except it fuses much more nicely.

-- basics

-- | \(O(1)\). The empty priority queue.
empty :: MinQueue a
empty = Empty

-- | \(O(1)\). Is this the empty priority queue?
null :: MinQueue a -> Bool
null Empty = True
null _     = False

-- | \(O(1)\). The number of elements in the queue.
size :: MinQueue a -> Int
size Empty            = 0
size (MinQueue n _ _) = n

-- | \(O(1)\). Returns the minimum element of the queue, if the queue is nonempty.
getMin :: MinQueue a -> Maybe a
getMin (MinQueue _ x _) = Just x
getMin _                = Nothing

-- | Retrieves the minimum element of the queue, and the queue stripped of that element,
-- or 'Nothing' if passed an empty queue.
minView :: Ord a => MinQueue a -> Maybe (a, MinQueue a)
minView Empty = Nothing
minView (MinQueue n x ts) = Just (x, case BQ.minView ts of
  Nothing        -> Empty
  Just (x', ts') -> MinQueue (n - 1) x' ts')

-- | \(O(1)\). Construct a priority queue with a single element.
singleton :: a -> MinQueue a
singleton x = MinQueue 1 x BQ.empty

-- | Amortized \(O(1)\), worst-case \(O(\log n)\). Insert an element into the priority queue.
insert :: Ord a => a -> MinQueue a -> MinQueue a
insert x Empty = singleton x
insert x (MinQueue n x' ts)
  | x <= x' = MinQueue (n + 1) x (BQ.insertMinQ x' ts)
  | otherwise = MinQueue (n + 1) x' (BQ.insert x ts)

-- | Amortized \(O(\log \min(n,m))\), worst-case \(O(\log \max(n,m))\). Take the union of two priority queues.
union :: Ord a => MinQueue a -> MinQueue a -> MinQueue a
union Empty q = q
union q Empty = q
union (MinQueue n1 x1 f1) (MinQueue n2 x2 f2)
  | x1 <= x2 = MinQueue (n1 + n2) x1 (BQ.unionPlusOne x2 f1 f2)
  | otherwise  = MinQueue (n1 + n2) x2 (BQ.unionPlusOne x1 f1 f2)


-- | Takes the union of a list of priority queues. Equivalent to @'foldl'' 'union' 'empty'@.
unions :: Ord a => [MinQueue a] -> MinQueue a
unions = foldl' union empty

-- | \(O(n)\). Map elements and collect the 'Just' results.
mapMaybe :: Ord b => (a -> Maybe b) -> MinQueue a -> MinQueue b
mapMaybe _ Empty = Empty
mapMaybe f (MinQueue _ x ts) = fromBare $ maybe q' (`BQ.insert` q') (f x)
  where
    q' = BQ.mapMaybe f ts

-- | \(O(n)\). Map elements and separate the 'Left' and 'Right' results.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MinQueue a -> (MinQueue b, MinQueue c)
mapEither _ Empty = (Empty, Empty)
mapEither f (MinQueue _ x ts)
  | (l, r) <- BQ.mapEither f ts
  = case f x of
      Left y -> (fromBare (BQ.insert y l), fromBare r)
      Right z -> (fromBare l, fromBare (BQ.insert z r))

-- | \(O(n)\). Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue,
-- as in 'fmap'. If it is not, the result is undefined.
mapMonotonic :: (a -> b) -> MinQueue a -> MinQueue b
mapMonotonic = mapU

{-# INLINABLE [0] foldrAsc #-}
-- | \(O(n \log n)\). Performs a right fold on the elements of a priority queue in
-- ascending order.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrAsc _ z Empty = z
foldrAsc f z (MinQueue _ x ts) = x `f` BQ.foldrUnfold f z BQ.minView ts

-- | \(O(n \log n)\). Performs a right fold on the elements of a priority queue in descending order.
-- @foldrDesc f z q == foldlAsc (flip f) z q@.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrDesc = foldlAsc . flip
{-# INLINE [0] foldrDesc #-}

-- | \(O(n \log n)\). Performs a left fold on the elements of a priority queue in
-- ascending order.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MinQueue a -> b
foldlAsc _ z Empty             = z
foldlAsc f z (MinQueue _ x ts) = BQ.foldlUnfold f (z `f` x) BQ.minView ts

{-# INLINABLE [1] toAscList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in ascending order.
toAscList :: Ord a => MinQueue a -> [a]
toAscList queue = foldrAsc (:) [] queue

{-# INLINABLE toAscListApp #-}
toAscListApp :: Ord a => MinQueue a -> [a] -> [a]
toAscListApp Empty app = app
toAscListApp (MinQueue _ x ts) app = x : BQ.foldrUnfold (:) app BQ.minView ts

{-# INLINABLE [1] toDescList #-}
-- | \(O(n \log n)\). Extracts the elements of the priority queue in descending order.
toDescList :: Ord a => MinQueue a -> [a]
toDescList queue = foldrDesc (:) [] queue

{-# INLINABLE toDescListApp #-}
toDescListApp :: Ord a => MinQueue a -> [a] -> [a]
toDescListApp Empty app = app
toDescListApp (MinQueue _ x ts) app = BQ.foldlUnfold (flip (:)) (x : app) BQ.minView ts

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

-- | @insertMinQ x h@ assumes that @x@ compares as less
-- than or equal to every element of @h@.
insertMinQ :: a -> MinQueue a -> MinQueue a
insertMinQ x Empty = singleton x
insertMinQ x (MinQueue n x' f) = MinQueue (n + 1) x (BQ.insertMinQ x' f)

-- | @insertMinQ' x h@ assumes that @x@ compares as less
-- than or equal to every element of @h@.
insertMinQ' :: a -> MinQueue a -> MinQueue a
insertMinQ' x Empty = singleton x
insertMinQ' x (MinQueue n x' f) = MinQueue (n + 1) x (BQ.insertMinQ' x' f)

-- | @insertMaxQ' x h@ assumes that @x@ compares as greater
-- than or equal to every element of @h@. It also assumes,
-- and preserves, an extra invariant. See 'insertMax'' for details.
-- tldr: this function can be used safely to build a queue from an
-- ascending list/array/whatever, but that's about it.
insertMaxQ' :: a -> MinQueue a -> MinQueue a
insertMaxQ' x Empty = singleton x
insertMaxQ' x (MinQueue n x' f) = MinQueue (n + 1) x' (BQ.insertMaxQ' x f)

{-# INLINABLE fromList #-}
-- | \(O(n)\). Constructs a priority queue from an unordered list.
fromList :: Ord a => [a] -> MinQueue a
-- We build a forest first and then extract its minimum at the end.
-- Why not just build the 'MinQueue' directly? This way saves us one
-- comparison per element.
fromList xs = fromBare (BQ.fromList xs)

mapU :: (a -> b) -> MinQueue a -> MinQueue b
mapU _ Empty = Empty
mapU f (MinQueue n x ts) = MinQueue n (f x) (BQ.mapU f ts)

{-# NOINLINE [0] foldrU #-}
-- | \(O(n)\). Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> MinQueue a -> b
foldrU _ z Empty = z
foldrU f z (MinQueue _ x ts) = x `f` BQ.foldrU f z ts

-- | \(O(n)\). Unordered left fold on a priority queue. This is rarely
-- what you want; 'foldrU' and 'foldlU'' are more likely to perform
-- well.
foldlU :: (b -> a -> b) -> b -> MinQueue a -> b
foldlU _ z Empty = z
foldlU f z (MinQueue _ x ts) = BQ.foldlU f (z `f` x) ts

-- | \(O(n)\). Unordered strict left fold on a priority queue.
--
-- @since 1.4.2
foldlU' :: (b -> a -> b) -> b -> MinQueue a -> b
foldlU' _ z Empty = z
foldlU' f z (MinQueue _ x ts) = BQ.foldlU' f (z `f` x) ts

-- | \(O(n)\). Unordered monoidal fold on a priority queue.
--
-- @since 1.4.2
foldMapU :: Monoid m => (a -> m) -> MinQueue a -> m
foldMapU _ Empty = mempty
foldMapU f (MinQueue _ x ts) = f x `mappend` BQ.foldMapU f ts

{-# NOINLINE toListU #-}
-- | \(O(n)\). Returns the elements of the queue, in no particular order.
toListU :: MinQueue a -> [a]
toListU q = foldrU (:) [] q

{-# NOINLINE toListUApp #-}
toListUApp :: MinQueue a -> [a] -> [a]
toListUApp Empty app = app
toListUApp (MinQueue _ x ts) app = x : BQ.foldrU (:) app ts

{-# RULES
"toListU/build" [~1] forall q. toListU q = build (\c n -> foldrU c n q)
"toListU" [1] forall q app. foldrU (:) app q = toListUApp q app
  #-}

-- traverseU :: Applicative f => (a -> f b) -> MinQueue a -> f (MinQueue b)
-- traverseU _ Empty = pure Empty
-- traverseU f (MinQueue n x ts) = MinQueue n <$> f x <*> traverse f ts

-- | \(O(\log n)\). @seqSpine q r@ forces the spine of @q@ and returns @r@.
--
-- Note: The spine of a 'MinQueue' is stored somewhat lazily. In earlier
-- versions of this package, some operations could produce chains of thunks
-- along the spine, occasionally necessitating manual forcing. Now, all
-- operations are careful to force enough to avoid this problem.
{-# DEPRECATED seqSpine "This function is no longer necessary or useful." #-}
seqSpine :: MinQueue a -> b -> b
seqSpine Empty z = z
seqSpine (MinQueue _ _ ts) z = BQ.seqSpine ts z

instance NFData a => NFData (MinQueue a) where
  rnf Empty             = ()
  rnf (MinQueue _ x ts) = x `deepseq` rnf ts

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
