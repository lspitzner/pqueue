module Validity.PQueue.Prio.Min
  ( validShape
  , validSize
  , validOrder
  ) where

import Data.PQueue.Prio.Internals as BQ
import qualified Validity.PQueue.Prio.BinomialQueue as VBQ

validShape :: MinPQueue k a -> Bool
validShape Empty = True
validShape (MinPQ _ _ _ f) = VBQ.validShapeF f

validSize :: MinPQueue k a -> Bool
validSize Empty = True
validSize (MinPQ sz _ _ f) = sz == sizeH f + 1

validOrder :: Ord k => MinPQueue k a -> Bool
validOrder Empty = True
validOrder (MinPQ _sz k _ f) = VBQ.precedesProperlyF k f

sizeH :: BinomHeap k a -> Int
sizeH = go 0 1
  where
    go :: Int -> Int -> BinomForest rk k a -> Int
    go acc rk Nil = rk `seq` acc
    go acc rk (Skip f) = go acc (2 * rk) f
    go acc rk (Cons _t f) = go (acc + rk) (2 * rk) f
