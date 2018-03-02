{-# LANGUAGE ScopedTypeVariables #-}
module AnaTreeSorted where 

import Data.Functor.Foldable 
import BinaryTree 

-- | Build a sorted Binary Tree from a list 
anaTreeSorted :: forall a. Ord a => [a] -> BinaryTree a 
anaTreeSorted = ana coalg where 
    coalg :: [a] -> BinaryTreeF a [a]
    coalg []       = LeafF
    coalg [x]      = NodeF [] x []
    coalg (x : xs) = NodeF (filter (<= x) xs) x (filter (> x) xs)

-- $
-- >>> anaTreeSorted [1,6,10,10,9,4,3, 5]
-- Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf)) 1 (Node (Node Leaf 6 Leaf) 5 (Node Leaf 7 Leaf))
