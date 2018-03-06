module ParaTreePaths where

import Data.Functor.Foldable
import BinaryTree

-- | List all the different paths from the root of a tree to its leafes
parapaths :: (BinaryTree a) -> [[a]]
parapaths = para alg where
    alg :: BinaryTreeF a (BinaryTree a, [[a]]) -> [[a]]
    alg LeafF = [[]]
    alg (NodeF (Leaf, _) v (Leaf, _)) = [[v]]                    -- avoid duplicates
    alg (NodeF (_, l) v (_, r)) = map (v : ) (l ++ r)

tree01 :: BinaryTree Int
tree01  = (Node (Node Leaf 1 Leaf) 5 (Node Leaf 3 Leaf))

tree02 :: BinaryTree Int
tree02  = (Node (Node (Node Leaf 2 Leaf) 1 Leaf) 5 (Node Leaf 3 Leaf))

-- >>> parapaths tree01
-- [[5,1],[5,3]]
-- >>> parapaths tree02
-- [[5,1,2],[5,1],[5,3]]
