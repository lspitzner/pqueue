import System.Random
import Gauge

import qualified KWay.PrioMergeAlg as KWay
import qualified PHeapSort as HS

kWay :: Int -> Int -> Benchmark
kWay i n = bench
  ("k-way merge looking " ++ show i ++ " deep into " ++ show n ++ " streams")
  (whnf ((!! i) . KWay.merge . KWay.mkStreams n) $ mkStdGen 5466122035931067691)

hSort :: Int -> Benchmark
hSort n = bench
  ("Heap sort with " ++ show n ++ " elements")
  (nf (HS.heapSortRandoms n) $ mkStdGen (-7750349139967535027))

main :: IO ()
main = defaultMainWith defaultConfig{timeLimit = Just 15}
  [ bgroup "heapSort"
      [ hSort (10^3)
      , hSort (10^4)
      , hSort (10^5)
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
  ]
