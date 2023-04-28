import System.Random
import Test.Tasty.Bench

import qualified KWay.MergeAlg as KWay
import qualified HeapSort as HS
import qualified Data.PQueue.Min as P

kWay :: Int -> Int -> Benchmark
kWay i n = bench
  (show i ++ " into " ++ show n ++ " streams")
  (whnf ((!! i) . KWay.merge . KWay.mkStreams n) $ mkStdGen 5466122035931067691)

hSort :: Int -> Benchmark
hSort n = bench
  ("Heap sort with " ++ show n ++ " elements")
  (nf (HS.heapSortRandoms n) $ mkStdGen (-7750349139967535027))

filterQ :: Int -> Benchmark
filterQ n = bench
  ("filter with " ++ show n ++ " elements")
  (whnf (P.drop 1 . P.filter (>0) . (P.fromList :: [Int] -> P.MinQueue Int) . take n . randoms) $ mkStdGen 977209486631198655)

partitionQ :: Int -> Benchmark
partitionQ n = bench
  ("partition with " ++ show n ++ " elements")
  (whnf (P.drop 1 . snd . P.partition (>0) . (P.fromList :: [Int] -> P.MinQueue Int) . take n . randoms) $ mkStdGen 781928047937198)

main :: IO ()
main = defaultMain [
    bgroup "heapSort"
      [ hSort (10^3)
      , hSort (10^4)
      , hSort (10^5)
      , hSort (10^6)
      , hSort (3*10^6)
      ]
  , bgroup "kWay"
      [ kWay (10^3) 1000000
      , kWay (10^5) 1000
      , kWay (10^5) 10000
      , kWay (10^5) 100000
      , kWay (10^6) 1000
      , kWay (10^6) 10000
      , kWay (10^6) 20000
      , kWay (3*10^6) 1000
      , kWay (2*10^6) 2000
      , kWay (4*10^6) 100
      ]
  , bgroup "filter"
      [ filterQ (10^3)
      , filterQ (10^4)
      , filterQ (10^5)
      , filterQ (10^6)
      , filterQ (3*10^6)
      ]
  , bgroup "partition"
      [ partitionQ (10^3)
      , partitionQ (10^4)
      , partitionQ (10^5)
      , partitionQ (10^6)
      , partitionQ (3*10^6)
      ]
  ]
