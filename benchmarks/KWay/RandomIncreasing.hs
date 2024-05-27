{-# language BangPatterns #-}
{-# language ViewPatterns #-}

module KWay.RandomIncreasing where

import System.Random
import Data.Word

data Stream = Stream !Word64 {-# UNPACK #-} !StdGen

viewStream :: Stream -> (Word64, Stream)
viewStream (Stream w gen) = (w, case uniform gen of (k, gen') -> Stream (w + fromIntegral (k :: Word16)) gen')

mkStream :: StdGen -> (Stream, StdGen)
mkStream gen
  | (gen1, gen2) <- split gen
  , (w16, gen1') <- uniform gen1
  = (Stream (fromIntegral (w16 :: Word16)) gen1', gen2)

mkStreams :: Int -> StdGen -> [Stream]
mkStreams !n !gen
  | n <= 0 = []
  | (s, gen') <- mkStream gen
  = s : mkStreams (n - 1) gen'
