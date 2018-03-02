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
-- >>> anaTreeSorted [6,10,10,9,4,5,1,3]
-- Node (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 4 (Node Leaf 5 Leaf)) 6 (Node (Node (Node Leaf 9 Leaf) 10 Leaf) 10 Leaf)