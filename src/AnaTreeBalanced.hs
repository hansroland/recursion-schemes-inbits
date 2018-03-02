{-# LANGUAGE ScopedTypeVariables #-}
module AnaTreeBalanced where 

import Data.Functor.Foldable 
import BinaryTree 

-- | Build a balanced Binary Tree from a list 
anaTreeBalanced :: forall a.[a] -> BinaryTree a 
anaTreeBalanced = ana coalg where 
    coalg :: [a] -> BinaryTreeF a [a]
    coalg []       = LeafF
    coalg [x]      = NodeF [] x []
    coalg (x : xs) = NodeF left x right 
      where
        (left, right) = splitAt (lh xs) xs
        lh :: [a] -> Int
        lh = (`div` 2) . length

-- $
-- >>> anaTreeBalanced [1,2,3,4,5,6,7]
-- Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf)) 1 (Node (Node Leaf 6 Leaf) 5 (Node Leaf 7 Leaf))
-- >>> anaTreeBalanced []
-- Leaf