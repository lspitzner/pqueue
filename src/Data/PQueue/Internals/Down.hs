{-# LANGUAGE CPP #-}

module Data.PQueue.Internals.Down
  ( getDown
  ) where
import Data.Ord (Down (..))

#if !MIN_VERSION_base(4,14,0)
getDown :: Down a -> a
getDown (Down a) = a
#endif
