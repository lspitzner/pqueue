module HeapSort where

import qualified Data.PQueue.Min as P
import System.Random

heapSortRandoms :: Int -> StdGen -> [Int]
heapSortRandoms n gen = heapSort $ take n (randoms gen)

heapSort :: Ord a => [a] -> [a]
heapSort = P.toAscList . P.fromList
