{-# language CPP #-}

{-# language BangPatterns #-}
{-# language ExtendedDefaultRules #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module Main (main) where

import Data.Bifunctor (bimap, first, second)
import qualified Data.Either as Either
import Data.Function (on)
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Ord (Down(..))

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.PQueue.Max as Max
import qualified Data.PQueue.Min as Min
import qualified Data.PQueue.Prio.Max as PMax
import qualified Data.PQueue.Prio.Min as PMin
import qualified Validity.PQueue.Min as VMin
import qualified Validity.PQueue.Prio.Min as VPMin
import qualified Validity.PQueue.Prio.Max as VPMax

default (Int)

validMinQueue :: Ord a => Min.MinQueue a -> Property
validMinQueue q = VMin.validShape q .&&. VMin.validSize q .&&. VMin.validOrder q

validPMinQueue :: Ord k => PMin.MinPQueue k a -> Property
validPMinQueue q = VPMin.validShape q .&&. VPMin.validSize q .&&. VPMin.validOrder q

validPMaxQueue :: Ord k => PMax.MaxPQueue k a -> Property
validPMaxQueue q = VPMax.validShape q .&&. VPMax.validSize q .&&. VPMax.validOrder q

main :: IO ()
main = defaultMain $ testGroup "pqueue"
  [ testGroup "Data.PQueue.Min"
    [ testProperty "size" $ \xs -> Min.size (Min.fromList xs) === length xs
    , testGroup "getMin"
      [ testProperty "empty" $ Min.getMin Min.empty === Nothing
      , testProperty "non-empty" $ \(NonEmpty xs) -> Min.getMin (Min.fromList xs) === Just (minimum xs)
      ]
    , testProperty "minView" $ \xs -> case Min.minView (Min.fromList xs) of
        Nothing -> xs === []
        Just (the_min, xs') ->
           validMinQueue xs' .&&.
           the_min : Min.toList xs' === List.sort xs
    , testProperty "insert" $ \x xs -> Min.insert x (Min.fromList xs) === Min.fromList (x : xs)
    , testProperty "union" $ \xs ys -> Min.union (Min.fromList xs) (Min.fromList ys) === Min.fromList (xs ++ ys)
    , testProperty "filter" $ \xs ->
        let xs' = Min.filter even (Min.fromList xs)
        in validMinQueue xs' .&&.
           Min.toList xs' === List.sort (List.filter even xs)
    , testProperty "partition" $ \xs ->
        let xs' = Min.fromList xs
            (ys, zs) = Min.partition even xs'
        in validMinQueue ys .&&.
           validMinQueue zs .&&.
           (Min.toList ys, Min.toList zs) === bimap List.sort List.sort (List.partition even xs)
    , testProperty "mapMaybe" $ \(Fn f) xs ->
        let xs' :: Min.MinQueue Char
            xs' = Min.mapMaybe f (Min.fromList xs)
        in validMinQueue xs' .&&.
           Min.toList xs' === List.sort (Maybe.mapMaybe f xs)
    , testProperty "mapEither" $ \(Fn f) xs ->
        let (ys, zs) = Min.mapEither f (Min.fromList xs)
        in validMinQueue ys .&&.
           validMinQueue zs .&&.
           (Min.toList ys, Min.toList zs) === bimap List.sort List.sort (Either.partitionEithers . List.map f $ xs)
    , testProperty "map" $ \xs -> Min.map negate (Min.fromList xs) === Min.fromList (List.map negate xs)
    , testProperty "take" $ \n xs -> Min.take n (Min.fromList xs) === List.take n (List.sort xs)
    , testProperty "drop" $ \n xs -> Min.drop n (Min.fromList xs) === Min.fromList (List.drop n (List.sort xs))
    , testProperty "splitAt" $ \n xs -> Min.splitAt n (Min.fromList xs) === second Min.fromList (List.splitAt n (List.sort xs))
    , testProperty "takeWhile" $ \(Fn f) xs -> Min.takeWhile f (Min.fromList xs) === List.takeWhile f (List.sort xs)
    , testProperty "dropWhile" $ \(Fn f) xs -> Min.dropWhile f (Min.fromList xs) === Min.fromList (List.dropWhile f (List.sort xs))
    , testProperty "span" $ \(Fn f) xs -> Min.span f (Min.fromList xs) === second Min.fromList (List.span f (List.sort xs))
    , testProperty "foldrAsc" $ \xs -> Min.foldrAsc (:) [] (Min.fromList xs) === List.sort xs
    , testProperty "foldlAsc" $ \xs -> Min.foldlAsc (flip (:)) [] (Min.fromList xs) === List.sortOn Down xs
    , testProperty "foldrDesc" $ \xs -> Min.foldrDesc (:) [] (Min.fromList xs) === List.sortOn Down xs
    , testProperty "foldlDesc" $ \xs -> Min.foldlDesc (flip (:)) [] (Min.fromList xs) === List.sort xs
    , testProperty "toAscList" $ \xs -> Min.toAscList (Min.fromList xs) === List.sort xs
    , testProperty "toDescList" $ \xs -> Min.toDescList (Min.fromList xs) === List.sortOn Down xs
    , testProperty "fromAscList" $ \xs -> Min.fromAscList (List.sort xs) === Min.fromList xs
    , testProperty "fromDescList" $ \xs -> Min.fromDescList (List.sortOn Down xs) === Min.fromList xs
    , testProperty "mapU" $ \xs ->
        let
          -- Monotonic, but not strictly so
          fun x
            | even x = x
            | otherwise = x + 1
          res = Min.mapU fun (Min.fromList xs)
        in validMinQueue res .&&. Min.toList res === List.map fun (List.sort xs)
    , testProperty "foldrU" $ \xs -> Min.foldrU (+) 0 (Min.fromList xs) === sum xs
    , testProperty "foldlU" $ \xs -> Min.foldlU (+) 0 (Min.fromList xs) === sum xs
    , testProperty "foldlU'" $ \xs -> Min.foldlU' (+) 0 (Min.fromList xs) === sum xs
    , testProperty "toListU" $ \xs -> List.sort (Min.toListU (Min.fromList xs)) === List.sort xs
    , testProperty "==" $ \(xs :: [(Int, ())]) ys -> ((==) `on` Min.fromList) xs ys === ((==) `on` List.sort) xs ys
    , testProperty "compare" $ \(xs :: [(Int, ())]) ys -> (compare `on` Min.fromList) xs ys === (compare `on` List.sort) xs ys
    ]
  , testGroup "Data.PQueue.Max"
    [ testProperty "size" $ \xs -> Max.size (Max.fromList xs) === length xs
    , testGroup "getMax"
      [ testProperty "empty" $ Max.getMax Max.empty === Nothing
      , testProperty "non-empty" $ \(NonEmpty xs) -> Max.getMax (Max.fromList xs) === Just (maximum xs)
      ]
    , testProperty "minView" $ \xs -> Max.maxView (Max.fromList xs) === fmap (second Max.fromList) (List.uncons (List.sortOn Down xs))
    , testProperty "insert" $ \x xs -> Max.insert x (Max.fromList xs) === Max.fromList (x : xs)
    , testProperty "union" $ \xs ys -> Max.union (Max.fromList xs) (Max.fromList ys) === Max.fromList (xs ++ ys)
    , testProperty "filter" $ \xs -> Max.filter even (Max.fromList xs) === Max.fromList (List.filter even xs)
    , testProperty "partition" $ \xs -> Max.partition even (Max.fromList xs) === bimap Max.fromList Max.fromList (List.partition even xs)
    , testProperty "map" $ \xs -> Max.map negate (Max.fromList xs) === Max.fromList (List.map negate xs)
    , testProperty "take" $ \n xs -> Max.take n (Max.fromList xs) === List.take n (List.sortOn Down xs)
    , testProperty "drop" $ \n xs -> Max.drop n (Max.fromList xs) === Max.fromList (List.drop n (List.sortOn Down xs))
    , testProperty "splitAt" $ \n xs -> Max.splitAt n (Max.fromList xs) === second Max.fromList (List.splitAt n (List.sortOn Down xs))
    , testProperty "takeWhile" $ \(Fn f) xs -> Max.takeWhile f (Max.fromList xs) === List.takeWhile f (List.sortOn Down xs)
    , testProperty "dropWhile" $ \(Fn f) xs -> Max.dropWhile f (Max.fromList xs) === Max.fromList (List.dropWhile f (List.sortOn Down xs))
    , testProperty "span" $ \(Fn f) xs -> Max.span f (Max.fromList xs) === second Max.fromList (List.span f (List.sortOn Down xs))
    , testProperty "foldrAsc" $ \xs -> Max.foldrAsc (:) [] (Max.fromList xs) === List.sort xs
    , testProperty "foldlAsc" $ \xs -> Max.foldlAsc (flip (:)) [] (Max.fromList xs) === List.sortOn Down xs
    , testProperty "foldrDesc" $ \xs -> Max.foldrDesc (:) [] (Max.fromList xs) === List.sortOn Down xs
    , testProperty "foldlDesc" $ \xs -> Max.foldlDesc (flip (:)) [] (Max.fromList xs) === List.sort xs
    , testProperty "toAscList" $ \xs -> Max.toAscList (Max.fromList xs) === List.sort xs
    , testProperty "toDescList" $ \xs -> Max.toDescList (Max.fromList xs) === List.sortOn Down xs
    , testProperty "fromAscList" $ \xs -> Max.fromAscList (List.sort xs) === Max.fromList xs
    , testProperty "fromDescList" $ \xs -> Max.fromDescList (List.sortOn Down xs) === Max.fromList xs
    , testProperty "mapU" $ \xs -> Max.mapU (+ 1) (Max.fromList xs) === Max.fromList (List.map (+ 1) xs)
    , testProperty "foldrU" $ \xs -> Max.foldrU (+) 0 (Max.fromList xs) === sum xs
    , testProperty "foldlU" $ \xs -> Max.foldlU (+) 0 (Max.fromList xs) === sum xs
    , testProperty "foldlU'" $ \xs -> Max.foldlU' (+) 0 (Max.fromList xs) === sum xs
    , testProperty "toListU" $ \xs -> List.sort (Max.toListU (Max.fromList xs)) === List.sort xs
    , testProperty "==" $ \(xs :: [(Int, ())]) ys -> ((==) `on` Max.fromList) xs ys === ((==) `on` List.sort) xs ys
    , testProperty "compare" $ \(xs :: [(Int, ())]) ys -> (compare `on` Max.fromList) xs ys === (compare `on` (List.sort . List.map Down)) xs ys
    ]
  , testGroup "Data.PQueue.Prio.Min"
    [ testProperty "size" $ \xs -> PMin.size (PMin.fromList xs) === length xs
    , testGroup "getMin"
      [ testProperty "empty" $ PMin.getMin PMin.empty === Nothing
      , testProperty "non-empty" $ \(NonEmpty xs) -> fmap fst (PMin.getMin (PMin.fromList xs)) === Just (fst (minimum xs))
      ]
    , testProperty "adjustMin" $ \xs -> PMin.adjustMin id (PMin.fromList xs) === PMin.fromList xs
    , testProperty "adjustMinA" $ \xs -> PMin.adjustMinA Identity (PMin.fromList xs) === Identity (PMin.fromList xs)
    , testGroup "updateMin"
      [ testProperty "Just" $ \xs -> PMin.updateMin Just (PMin.fromList xs) === PMin.fromList xs
      , testProperty "Nothing" $ \(NonEmpty (xs :: [(Int, ())])) -> PMin.updateMin (const Nothing) (PMin.fromList xs) === PMin.fromList (tail (List.sort xs))
      ]
    , testGroup "updateMinA"
      [ testProperty "Just" $ \xs -> PMin.updateMinA (Identity . Just) (PMin.fromList xs) === Identity (PMin.fromList xs)
      , testProperty "Nothing" $ \(NonEmpty (xs :: [(Int, ())])) -> PMin.updateMinA (Identity . const Nothing) (PMin.fromList xs) === Identity (PMin.fromList (tail (List.sort xs)))
      ]
    , testProperty "minViewWithKey" $ \(xs :: [(Int, Int)]) -> case PMin.minViewWithKey (PMin.fromList xs) of
        Nothing -> xs === []
        Just ((the_min, the_min_val), xs') ->
           validPMinQueue xs' .&&.
           List.sort ((the_min, the_min_val) : PMin.toList xs') === List.sort xs
    , testProperty "map" $ \(xs :: [(Int, ())]) -> PMin.map id (PMin.fromList xs) === PMin.fromList xs
    , testProperty "mapKeysMonotonic" $ \xs ->
        let
          -- Monotonic, but not strictly so
          fun x
            | even x = x
            | otherwise = x + 1
          res = PMin.mapKeysMonotonic fun (PMin.fromList xs)
        in validPMinQueue res .&&. List.sort (PMin.toList res) === List.sort (List.map (first fun) xs)
    , testProperty "take" $ \n (xs :: [(Int, ())]) -> PMin.take n (PMin.fromList xs) === List.take n (List.sort xs)
    , testProperty "drop" $ \n (xs :: [(Int, ())]) -> PMin.drop n (PMin.fromList xs) === PMin.fromList (List.drop n (List.sort xs))
    , testProperty "splitAt" $ \n (xs :: [(Int, ())]) -> PMin.splitAt n (PMin.fromList xs) === second PMin.fromList (List.splitAt n (List.sort xs))
    , testProperty "takeWhile" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMin.takeWhileWithKey f (PMin.fromList xs) === List.takeWhile (uncurry f) (List.sort xs)
    , testProperty "dropWhile" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMin.dropWhileWithKey f (PMin.fromList xs) === PMin.fromList (List.dropWhile (uncurry f) (List.sort xs))
    , testProperty "span" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMin.spanWithKey f (PMin.fromList xs) === second PMin.fromList (List.span (uncurry f) (List.sort xs))
    , testProperty "foldrWithKey" $ \(xs :: [(Int, ())]) -> PMin.foldrWithKey (\k x acc -> (k, x) : acc) [] (PMin.fromList xs) === List.sort xs
    , testProperty "foldlWithKey" $ \(xs :: [(Int, ())]) -> PMin.foldlWithKey (\acc k x -> (k, x) : acc) [] (PMin.fromList xs) === List.sortOn Down xs
    , testProperty "traverseWithKey" $
      \(Fn2 (f :: Int -> () -> Maybe ())) (xs :: [(Int, ())]) -> PMin.traverseWithKey f (PMin.fromList xs) === fmap PMin.fromList (traverse (\(k, x) -> fmap (k,) (f k x)) xs)
    , testProperty "mapMWithKey" $
      \(Fn2 (f :: Int -> () -> Maybe ())) (xs :: [(Int, ())]) -> PMin.mapMWithKey f (PMin.fromList xs) === fmap PMin.fromList (traverse (\(k, x) -> fmap (k,) (f k x)) xs)
    , testProperty "insert" $ \k xs -> PMin.insert k () (PMin.fromList xs) === PMin.fromList ((k, ()) : xs)
    , testProperty "union" $ \(xs :: [(Int, ())]) ys -> PMin.union (PMin.fromList xs) (PMin.fromList ys) === PMin.fromList (xs ++ ys)
    , testProperty "filter" $ \(xs :: [(Int, Int)]) ->
        let
          -- The probability of a number not being divisible by 3 is 2/3.
          -- The probability of a number not being divisible by 4 is 3/4.
          -- So the probability of a number being divisible by neither is
          -- 1/2.
          f x y = x `rem` 3 == 0 || y `rem` 4 == 0
          xs' = PMin.filterWithKey f (PMin.fromList xs)
        in validPMinQueue xs' .&&.
           List.sort (PMin.toList xs') === List.sort (List.filter (uncurry f) xs)
    , testProperty "partition" $ \(xs :: [(Int, Int)]) ->
        let
          f x y = x `rem` 3 == 0 || y `rem` 4 == 0
          (ys, zs) = PMin.partitionWithKey f (PMin.fromList xs)
        in validPMinQueue ys .&&.
           validPMinQueue zs .&&.
           (List.sort (PMin.toList ys), List.sort (PMin.toList zs)) ===
             bimap List.sort List.sort (List.partition (uncurry f) xs)
    , testProperty "mapMaybe" $ \(Fn2 f) (xs :: [(Int, Int)]) ->
        let
          xs' = PMin.mapMaybeWithKey f (PMin.fromList xs)
        in validPMinQueue xs' .&&.
           List.sort (PMin.toList xs') === List.sort (Maybe.mapMaybe (\(k,v) -> fmap (k,) (f k v)) xs)
    , testProperty "mapEither" $ \(Fn2 f) (xs :: [(Int, Int)]) ->
        let (ys, zs) = PMin.mapEitherWithKey f (PMin.fromList xs)
        in validPMinQueue ys .&&.
           validPMinQueue zs .&&.
           (List.sort (PMin.toList ys), List.sort (PMin.toList zs)) ===
             bimap List.sort List.sort (Either.partitionEithers . List.map (\(k,v) -> bimap (k,) (k,) (f k v)) $ xs)
    , testProperty "toAscList" $ \(xs :: [(Int, ())]) -> PMin.toAscList (PMin.fromList xs) === List.sort xs
    , testProperty "toDescList" $ \(xs :: [(Int, ())]) -> PMin.toDescList (PMin.fromList xs) === List.sortOn Down xs
    , testProperty "fromAscList" $ \(xs :: [(Int, ())]) -> PMin.fromAscList (List.sort xs) === PMin.fromList xs
    , testProperty "fromDescList" $ \(xs :: [(Int, ())]) -> PMin.fromDescList (List.sortOn Down xs) === PMin.fromList xs
    , testProperty "foldrU" $ \xs -> PMin.foldrU (+) 0 (PMin.fromList xs) === sum (List.map snd xs)
    , testProperty "foldlU" $ \xs -> PMin.foldlU (+) 0 (PMin.fromList xs) === sum (List.map snd xs)
    , testProperty "foldlU'" $ \xs -> PMin.foldlU' (+) 0 (PMin.fromList xs) === sum (List.map snd xs)
    , testProperty "traverseU" $
      \(Fn (f :: () -> Maybe ())) (xs :: [(Int, ())]) -> PMin.traverseU f (PMin.fromList xs) === fmap PMin.fromList (traverse (\(k, x) -> fmap (k,) (f x)) xs)
    , testProperty "toListU" $ \xs -> List.sort (PMin.toListU (PMin.fromList xs)) === List.sort xs
    , testProperty "==" $ \(xs :: [(Int, ())]) ys -> ((==) `on` PMin.fromList) xs ys === ((==) `on` List.sort) xs ys
    , testProperty "compare" $ \(xs :: [(Int, ())]) ys -> (compare `on` PMin.fromList) xs ys === (compare `on` List.sort) xs ys
    ]
  , testGroup "Data.PQueue.Prio.Max"
    [ testProperty "size" $ \xs -> PMax.size (PMax.fromList xs) === length xs
    , testGroup "getMax"
      [ testProperty "empty" $ PMax.getMax PMax.empty === Nothing
      , testProperty "non-empty" $ \(NonEmpty xs) -> fmap fst (PMax.getMax (PMax.fromList xs)) === Just (fst (maximum xs))
      ]
    , testProperty "adjustMin" $ \xs -> PMax.adjustMax id (PMax.fromList xs) === PMax.fromList xs
    , testProperty "adjustMinA" $ \xs -> PMax.adjustMaxA Identity (PMax.fromList xs) === Identity (PMax.fromList xs)
    , testGroup "updateMin"
      [ testProperty "Just" $ \xs -> PMax.updateMax Just (PMax.fromList xs) === PMax.fromList xs
      , testProperty "Nothing" $ \(NonEmpty (xs :: [(Int, ())])) -> PMax.updateMax (const Nothing) (PMax.fromList xs) === PMax.fromList (tail (List.sortOn Down xs))
      ]
    , testGroup "updateMinA"
      [ testProperty "Just" $ \xs -> PMax.updateMaxA (Identity . Just) (PMax.fromList xs) === Identity (PMax.fromList xs)
      , testProperty "Nothing" $ \(NonEmpty (xs :: [(Int, ())])) -> PMax.updateMaxA (Identity . const Nothing) (PMax.fromList xs) === Identity (PMax.fromList (tail (List.sortOn Down xs)))
      ]
    , testProperty "minViewWithKey" $ \(xs :: [(Int, ())]) -> PMax.maxViewWithKey (PMax.fromList xs) === fmap (second PMax.fromList) (List.uncons (List.sortOn Down xs))
    , testProperty "map" $ \(xs :: [(Int, ())]) -> PMax.map id (PMax.fromList xs) === PMax.fromList xs
    , testProperty "mapKeysMonotonic" $ \xs ->
        let
          -- Monotonic, but not strictly so
          fun x
            | even x = x
            | otherwise = x + 1
          res = PMax.mapKeysMonotonic fun (PMax.fromList xs)
        in validPMaxQueue res .&&. List.sort (PMax.toList res) === List.sort (List.map (first fun) xs)
    , testProperty "take" $ \n (xs :: [(Int, ())]) -> PMax.take n (PMax.fromList xs) === List.take n (List.sortOn Down xs)
    , testProperty "drop" $ \n (xs :: [(Int, ())]) -> PMax.drop n (PMax.fromList xs) === PMax.fromList (List.drop n (List.sortOn Down xs))
    , testProperty "splitAt" $ \n (xs :: [(Int, ())]) -> PMax.splitAt n (PMax.fromList xs) === second PMax.fromList (List.splitAt n (List.sortOn Down xs))
    , testProperty "takeWhile" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMax.takeWhileWithKey f (PMax.fromList xs) === List.takeWhile (uncurry f) (List.sortOn Down xs)
    , testProperty "dropWhile" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMax.dropWhileWithKey f (PMax.fromList xs) === PMax.fromList (List.dropWhile (uncurry f) (List.sortOn Down xs))
    , testProperty "span" $ \(Fn2 f) (xs :: [(Int, ())]) -> PMax.spanWithKey f (PMax.fromList xs) === second PMax.fromList (List.span (uncurry f) (List.sortOn Down xs))
    , testProperty "foldrWithKey" $ \(xs :: [(Int, ())]) -> PMax.foldrWithKey (\k x acc -> (k, x) : acc) [] (PMax.fromList xs) === List.sortOn Down xs
    , testProperty "foldlWithKey" $ \(xs :: [(Int, ())]) -> PMax.foldlWithKey (\acc k x -> (k, x) : acc) [] (PMax.fromList xs) === List.sort xs
    , testProperty "traverseWithKey" $
      \(Fn2 (f :: Int -> () -> Maybe ())) (xs :: [(Int, ())]) -> PMax.traverseWithKey f (PMax.fromList xs) === fmap PMax.fromList (traverse (\(k, x) -> fmap (k,) (f k x)) xs)
    , testProperty "mapMWithKey" $
      \(Fn2 (f :: Int -> () -> Maybe ())) (xs :: [(Int, ())]) -> PMax.mapMWithKey f (PMax.fromList xs) === fmap PMax.fromList (traverse (\(k, x) -> fmap (k,) (f k x)) xs)
    , testProperty "insert" $ \k xs -> PMax.insert k () (PMax.fromList xs) === PMax.fromList ((k, ()) : xs)
    , testProperty "union" $ \(xs :: [(Int, ())]) ys -> PMax.union (PMax.fromList xs) (PMax.fromList ys) === PMax.fromList (xs ++ ys)
    , testProperty "filter" $
      \(xs :: [(Int, ())]) -> PMax.filterWithKey (\k _ -> even k) (PMax.fromList xs) === PMax.fromList (List.filter (even . fst) xs)
    , testProperty "partition" $
      \(xs :: [(Int, ())]) -> PMax.partitionWithKey (\k _ -> even k) (PMax.fromList xs) === bimap PMax.fromList PMax.fromList (List.partition (even . fst) xs)
    , testProperty "toAscList" $ \(xs :: [(Int, ())]) -> PMax.toAscList (PMax.fromList xs) === List.sort xs
    , testProperty "toDescList" $ \(xs :: [(Int, ())]) -> PMax.toDescList (PMax.fromList xs) === List.sortOn Down xs
    , testProperty "fromAscList" $ \(xs :: [(Int, ())]) -> PMax.fromAscList (List.sort xs) === PMax.fromList xs
    , testProperty "fromDescList" $ \(xs :: [(Int, ())]) -> PMax.fromDescList (List.sortOn Down xs) === PMax.fromList xs
    , testProperty "foldrU" $ \xs -> PMax.foldrU (+) 0 (PMax.fromList xs) === sum (List.map snd xs)
    , testProperty "foldlU" $ \xs -> PMax.foldlU (+) 0 (PMax.fromList xs) === sum (List.map snd xs)
    , testProperty "foldlU'" $ \xs -> PMax.foldlU' (+) 0 (PMax.fromList xs) === sum (List.map snd xs)
    , testProperty "traverseU" $
      \(Fn (f :: () -> Maybe ())) (xs :: [(Int, ())]) -> PMax.traverseU f (PMax.fromList xs) === fmap PMax.fromList (traverse (\(k, x) -> fmap (k,) (f x)) xs)
    , testProperty "toListU" $ \xs -> List.sort (PMax.toListU (PMax.fromList xs)) === List.sort xs
    , testProperty "==" $ \(xs :: [(Int, ())]) ys -> ((==) `on` PMax.fromList) xs ys === ((==) `on` List.sort) xs ys
    , testProperty "compare" $ \(xs :: [(Int, ())]) ys -> (compare `on` PMax.fromList) xs ys === (compare `on` (List.sort . List.map Down)) xs ys
    ]
  ]
