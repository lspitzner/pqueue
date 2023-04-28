-- | Validity testing
module Validity.BinomialQueue
  ( validShape
  , precedesProperly
  ) where

import BinomialQueue.Internals

-- | Does the heap have a valid shape?
validShape :: MinQueue a -> Bool
validShape (MinQueue f) = validShapeF f
  
validShapeF :: BinomForest rk a -> Bool
validShapeF (Cons _ f) = validShapeF f
validShapeF (Skip Nil) = False
validShapeF (Skip _f) = True
validShapeF Nil = True
  
-- | Takes an element and a priority queue. Checks that the queue is in heap
-- order and that the element is less than or equal to all elements of the
-- queue.
precedesProperly :: Ord a => a -> MinQueue a -> Bool
precedesProperly a (MinQueue q) = precedesProperlyF a q
  
-- | Takes an element and a forest. Checks that the forest is in heap order
-- and that the element is less than or equal to all elements of the forest.
precedesProperlyF :: (Ord a, TreeValidity rk) => a -> BinomForest rk a -> Bool
precedesProperlyF _ Nil = True
precedesProperlyF the_min (Skip f) = precedesProperlyF the_min f
precedesProperlyF the_min (Cons t ts) = precedesProperlyTree the_min t
  && precedesProperlyF the_min ts
  
-- | Takes an element and a tree. Checks that the tree is in heap order
-- and that the element is less than or equal to all elements of the tree.
precedesProperlyTree :: (Ord a, TreeValidity rk) => a -> BinomTree rk a -> Bool
precedesProperlyTree the_min (BinomTree a ts) = the_min <= a && precedesProperlyRk a ts
  
-- | A helper class for order validity checking
class TreeValidity rk where
  -- | Takes an element and a collection of trees. Checks that the collection
  -- is in heap order and that the element is less than or equal to all
  -- elements of the collection.
  precedesProperlyRk :: Ord a => a -> rk a -> Bool
instance TreeValidity Zero where
  precedesProperlyRk _ ~Zero = True
instance TreeValidity rk => TreeValidity (Succ rk) where
  precedesProperlyRk the_min (Succ t q) =
    precedesProperlyTree the_min t &&
    precedesProperlyRk the_min q
