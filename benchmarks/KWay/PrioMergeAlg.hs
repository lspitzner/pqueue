{-# language BangPatterns #-}
{-# language ViewPatterns #-}

module KWay.PrioMergeAlg
  ( merge
  , mkStreams
  ) where

import qualified Data.PQueue.Prio.Min as P
import Data.Word
import Data.List (unfoldr)
import KWay.RandomIncreasing

type PQ = P.MinPQueue

merge :: [Stream] -> [Word64]
merge = unfoldr go . P.fromList . map viewStream
  where
    go :: PQ Word64 Stream -> Maybe (Word64, PQ Word64 Stream)
    go (P.minViewWithKey -> Just ((a, viewStream -> (b, s)), ss))
      = Just (a, P.insert b s ss)
    go _ = Nothing
