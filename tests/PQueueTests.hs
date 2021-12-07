module Main (main) where

import qualified Data.PQueue.Prio.Max as PMax
import qualified Data.PQueue.Prio.Min as PMin
import qualified Data.PQueue.Max as Max ()
import qualified Data.PQueue.Min as Min

import Test.QuickCheck
import Test.QuickCheck.Poly (OrdA, B, C)
import Test.QuickCheck.Function (Fun, applyFun2)

import System.Exit

import qualified Data.List as List
import Data.Function (on)
import Control.Arrow (second)


validMinToAscList :: [Int] -> Bool
validMinToAscList xs = Min.toAscList (Min.fromList xs) == List.sort xs

validMinPrioToAscList :: [(Int,OrdA)] -> Bool
validMinPrioToAscList xs =
  List.concatMap List.sort (List.groupBy ((==) `on` fst) (PMin.toAscList (PMin.fromList xs))) == List.sort xs

validMinToDescList :: [Int] -> Bool
validMinToDescList xs = Min.toDescList (Min.fromList xs) == List.sortBy (flip compare) xs

validMinUnfoldr :: [Int] -> Bool
validMinUnfoldr xs = List.unfoldr Min.minView (Min.fromList xs) == List.sort xs

validMinToList :: [Int] -> Bool
validMinToList xs = List.sort (Min.toList (Min.fromList xs)) == List.sort xs

validMinFromAscList :: [Int] -> Bool
validMinFromAscList xs = Min.fromAscList (List.sort xs) == Min.fromList xs

validMinPrioFromAscList :: [(Int, OrdA)] -> Bool
validMinPrioFromAscList xs =
  List.concatMap List.sort (List.groupBy ((==) `on` fst) (PMin.toAscList (PMin.fromAscList sorted))) == sorted
  where sorted = List.sort xs

validMinFromDescList :: [Int] -> Bool
validMinFromDescList xs = Min.fromDescList (List.sortBy (flip compare) xs) == Min.fromList xs

validMinPrioFromDescList :: [(Int, OrdA)] -> Property
validMinPrioFromDescList xs =
  List.concatMap List.sort (List.groupBy ((==) `on` fst) (PMin.toAscList (PMin.fromDescList sorted))) === reverse sorted
  where sorted = bsort xs
        bsort = List.sortBy (flip compare)

validMinUnion :: [Int] -> [Int] -> Bool
validMinUnion xs1 xs2 = Min.union (Min.fromList xs1) (Min.fromList xs2) == Min.fromList (xs1 ++ xs2)

validMinMapMonotonic :: [Int] -> Bool
validMinMapMonotonic xs = Min.mapU (+1) (Min.fromList xs) == Min.fromList (map (+1) xs)

validMinFilter :: [Int] -> Bool
validMinFilter xs = Min.filter even (Min.fromList xs) == Min.fromList (List.filter even xs)

validMinPartition :: [Int] -> Bool
validMinPartition xs = Min.partition even (Min.fromList xs) == (let (xs1, xs2) = List.partition even xs in (Min.fromList xs1, Min.fromList xs2))

validMinCmp :: [Int] -> [Int] -> Bool
validMinCmp xs1 xs2 = compare (Min.fromList xs1) (Min.fromList xs2) == compare (List.sort xs1) (List.sort xs2)

validMinCmp2 :: [Int] -> Bool
validMinCmp2 xs = compare (Min.fromList ys) (Min.fromList (take 30 ys)) == compare ys (take 30 ys)
  where ys = List.sort xs

validSpan :: [Int] -> Bool
validSpan xs = (Min.takeWhile even q, Min.dropWhile even q) == Min.span even q
  where q = Min.fromList xs

validSpan2 :: [Int] -> Bool
validSpan2 xs =
  second Min.toAscList (Min.span even (Min.fromList xs))
  ==
  List.span even (List.sort xs)

validSplit :: Int -> [Int] -> Bool
validSplit n xs = Min.splitAt n q == (Min.take n q, Min.drop n q)
  where q = Min.fromList xs

validSplit2 :: Int -> [Int] -> Bool
validSplit2 n xs = case Min.splitAt n (Min.fromList xs) of
  (ys, q') -> (ys, Min.toAscList q') == List.splitAt n (List.sort xs)

validMapEither :: [Int] -> Bool
validMapEither xs =
  Min.mapEither collatz q ==
    (Min.mapMaybe (either Just (const Nothing) . collatz) q,
     Min.mapMaybe (either (const Nothing) Just . collatz) q)
      where q = Min.fromList xs

validMap :: [Int] -> Bool
validMap xs = Min.map f (Min.fromList xs) == Min.fromList (List.map f xs)
  where f = either id id . collatz

collatz :: Int -> Either Int Int
collatz x =
  if even x
    then Left $ x `quot` 2
    else Right $ 3 * x + 1

validSize :: [Int] -> Bool
validSize xs = Min.size q == List.length xs'
  where
    q = Min.drop 10 (Min.fromList xs)
    xs' = List.drop 10 (List.sort xs)

validNull :: Int -> [Int] -> Bool
validNull n xs = Min.null q == List.null xs'
  where
    q = Min.drop n (Min.fromList xs)
    xs' = List.drop n (List.sort xs)

validFoldl :: [Int] -> Bool
validFoldl xs = Min.foldlAsc (flip (:)) [] (Min.fromList xs) == List.foldl (flip (:)) [] (List.sort xs)

validFoldlU :: [Int] -> Bool
validFoldlU xs = Min.foldlU (flip (:)) [] q == List.reverse (Min.foldrU (:) [] q)
  where q = Min.fromList xs

validFoldrU :: [Int] -> Bool
validFoldrU xs = Min.foldrU (+) 0 q == List.sum xs
  where q = Min.fromList xs

validTraverseWithKey :: Fun (OrdA, B) [C] -> Short (OrdA, B) -> Property
validTraverseWithKey f (Short xs) =
  PMin.traverseWithKey (applyFun2 f) q === PMin.mapMWithKey (applyFun2 f) q
  where
    q = PMin.fromList xs

validTraverseWithKeyMax :: Fun (OrdA, B) [C] -> Short (OrdA, B) -> Property
validTraverseWithKeyMax f (Short xs) =
  PMax.traverseWithKey (applyFun2 f) q === PMax.mapMWithKey (applyFun2 f) q
  where
    q = PMax.fromList xs

-- Lists of between 0 and 3 elements
newtype Short a = Short [a]
  deriving Show
instance Arbitrary a => Arbitrary (Short a) where
  arbitrary = do
    n <- frequency [(1, pure 0), (5, pure 1), (4, pure 2), (3, pure 3)]
    Short <$> vectorOf n arbitrary

main :: IO ()
main = do
  check validMinToAscList
  check validMinPrioToAscList
  check validMinToDescList
  check validMinUnfoldr
  check validMinToList
  check validMinFromAscList
  check validMinPrioFromAscList
  check validMinFromDescList
  check validMinPrioFromDescList
  check validMinUnion
  check validMinMapMonotonic
  check validMinPartition
  check validMinCmp
  check validMinCmp2
  check validSpan
  check validSpan2
  check validSplit
  check validSplit2
  check validMinFilter
  check validMapEither
  check validMap
  check validSize
  check validNull
  check validFoldl
  check validFoldlU
  check validFoldrU
  check validTraverseWithKey
  check validTraverseWithKeyMax
  putStrLn "all tests passed"

isPass :: Result -> Bool
isPass Success{} = True
isPass _         = False

check :: Testable prop => prop -> IO ()
check p = do
  r <- quickCheckResult p
  if isPass r then return () else exitFailure
