{-# language BangPatterns #-}
{-# language ViewPatterns #-}

module KWay.MergeAlg where

import qualified Data.PQueue.Min as P
import System.Random (StdGen)
import Data.Word
import Data.List (unfoldr)
import qualified KWay.RandomIncreasing as RI
import Data.Function (on)
import Data.Coerce

newtype Stream = Stream { unStream :: RI.Stream }

viewStream :: Stream -> (Word64, Stream)
viewStream = coerce RI.viewStream

instance Eq Stream where
  (==) = (==) `on` (fst . viewStream)

instance Ord Stream where
  compare = compare `on` (fst . viewStream)

type PQ = P.MinQueue

merge :: [Stream] -> [Word64]
merge = unfoldr go . P.fromList
  where
    go :: PQ Stream -> Maybe (Word64, PQ Stream)
    go (P.minView -> Just (viewStream -> (a, s), ss))
      = Just (a, P.insert s ss)
    go _ = Nothing

mkStreams :: Int -> StdGen -> [Stream]
mkStreams = coerce RI.mkStreams
