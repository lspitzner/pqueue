-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Prio.Max
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue.
-- Each element is associated with a /key/, and the priority queue supports
-- viewing and extracting the element with the maximum key.
--
-- A worst-case bound is given for each operation. In some cases, an amortized
-- bound is also specified; these bounds hold even in a persistent context.
--
-- This implementation is based on a binomial heap augmented with a global root.
--
-- We do not guarantee stable behavior.
-- Ties are broken arbitrarily -- that is, if @k1 <= k2@ and @k2 <= k1@, then there
-- are no guarantees about the relative order in which @k1@, @k2@, and their associated
-- elements are returned. (Unlike Data.Map, we allow multiple elements with the
-- same key.)
--
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered. No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue.Prio.Max (
  MaxPQueue,
  -- * Construction
  empty,
  singleton,
  insert,
  insertBehind,
  union,
  unions,
  -- * Query
  null,
  size,
  -- ** Maximum view
  findMax,
  getMax,
  deleteMax,
  deleteFindMax,
  adjustMax,
  adjustMaxWithKey,
  updateMax,
  updateMaxWithKey,
  maxView,
  maxViewWithKey,
  -- * Traversal
  -- ** Map
  map,
  mapWithKey,
  mapKeys,
  mapKeysMonotonic,
  -- ** Fold
  foldrWithKey,
  foldlWithKey,
  -- ** Traverse
  traverseWithKey,
  mapMWithKey,
  -- * Subsets
  -- ** Indexed
  take,
  drop,
  splitAt,
  -- ** Predicates
  takeWhile,
  takeWhileWithKey,
  dropWhile,
  dropWhileWithKey,
  span,
  spanWithKey,
  break,
  breakWithKey,
  -- *** Filter
  filter,
  filterWithKey,
  partition,
  partitionWithKey,
  mapMaybe,
  mapMaybeWithKey,
  mapEither,
  mapEitherWithKey,
  -- * List operations
  -- ** Conversion from lists
  fromList,
  fromAscList,
  fromDescList,
  -- ** Conversion to lists
  keys,
  elems,
  assocs,
  toAscList,
  toDescList,
  toList,
  -- * Unordered operations
  foldrU,
  foldrWithKeyU,
  foldMapWithKeyU,
  foldlU,
  foldlWithKeyU,
  foldlWithKeyU',
  traverseU,
  traverseWithKeyU,
  keysU,
  elemsU,
  assocsU,
  toListU,
  -- * Helper methods
  seqSpine
  )
  where

import Data.PQueue.Prio.Max.Internals
import Prelude ()
