module Validity.PQueue.Min
  ( validShape
  , validSize
  , validOrder
  ) where

import Data.PQueue.Internals
import qualified BinomialQueue.Internals as BQ
import qualified Validity.BinomialQueue as VBQ

validShape :: MinQueue a -> Bool
validShape Empty = True
validShape (MinQueue _ _ f) = VBQ.validShape f

validSize :: MinQueue a -> Bool
validSize Empty = True
validSize (MinQueue sz _ f) = sz == BQ.size f + 1

validOrder :: Ord a => MinQueue a -> Bool
validOrder Empty = True
validOrder (MinQueue _sz a f) = VBQ.precedesProperly a f
