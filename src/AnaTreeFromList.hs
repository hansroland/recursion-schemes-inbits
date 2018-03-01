{-# LANGUAGE ScopedTypeVariables #-}
module AnaTreeFromList where 

import Data.Functor.Foldable 
import BinaryTree 

-- | Build a Binary Tree from a list 
anaBuildTree :: forall a.[a] -> BinaryTree a 
anaBuildTree = ana coalg where 
    coalg :: [a] -> BinaryTreeF a [a]
    coalg []       = LeafF
    coalg [x]      = NodeF [] x []
    coalg (x : xs) = NodeF left x right 
      where
        (left, right) = splitAt (lh xs) xs
        lh :: [a] -> Int
        lh = (`div` 2) . length

-- $
-- >>> anaBuildTree [1,2,3,4,5,6,7]
-- Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf)) 1 (Node (Node Leaf 6 Leaf) 5 (Node Leaf 7 Leaf))
-- >>> anaBuildTree []
-- Leaf