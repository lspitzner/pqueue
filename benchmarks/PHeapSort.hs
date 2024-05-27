module PHeapSort where

import qualified Data.PQueue.Prio.Min as P
import System.Random

heapSortRandoms :: Int -> StdGen -> [Int]
heapSortRandoms n gen = heapSort $ take n (randoms gen)

heapSort :: Ord a => [a] -> [a]
heapSort xs = [b | (b, ~()) <- P.toAscList . P.fromList . map (\a -> (a, ())) $ xs]
