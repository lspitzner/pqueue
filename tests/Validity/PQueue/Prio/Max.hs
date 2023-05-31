module Validity.PQueue.Prio.Max
  ( validShape
  , validSize
  , validOrder
  ) where

import Data.PQueue.Prio.Max.Internals as PQM
import qualified Validity.PQueue.Prio.Min as VMin

validShape :: MaxPQueue k a -> Bool
validShape (MaxPQ q) = VMin.validShape q

validSize :: MaxPQueue k a -> Bool
validSize (MaxPQ q) = VMin.validSize q

validOrder :: Ord k => MaxPQueue k a -> Bool
validOrder (MaxPQ q) = VMin.validOrder q
