module ZygoTreePerfect where

import Data.Functor.Foldable
import BinaryTree
 
-- | Calculate the depth of a tree
depth :: BinaryTree a -> Int 
depth  = cata alg 
  where 
    alg :: BinaryTreeF a Int -> Int
    alg LeafF = 0 
    alg (NodeF l _ r) = 1 + max l r

-- | Perfect tree checker: version with direct recursion
-- | Problem: This version stops recursing the right subtree 
-- as soon as the left subtree is not perfect.
-- The direct version does two tree walks,one for perfect', one for depth 

perfect' :: BinaryTree a -> Bool 
perfect' Leaf = True 
perfect' (Node l _ r) = perfect' l && perfect' r && (depth l == depth r)

-- | Perfect tree checker: recursion-scheme version with zygomorphism
-- See: www.cs.ox.ac.uk/people/jeremy.gibbons/publications/urs.pdf 
perfect :: BinaryTree a -> Bool
perfect = zygo f1 f2 
   where 
    f1 :: BinaryTreeF a Int -> Int
    f1 LeafF  = 0 
    f1 (NodeF l _ r) = 1 + max l r
    f2 :: BinaryTreeF a (Int, Bool) -> Bool
    f2 LeafF  = True 
    f2 (NodeF (ln, lb) _ (rn, rb)) = ln == rn && (lb && rb)


-- Define some trees for our examples 
tree00 :: BinaryTree Int 
tree00 = Node (Node Leaf 10 Leaf) 30 (Node Leaf 40 Leaf )

tree01 :: BinaryTree Int 
tree01 = Node (Node Leaf 1 Leaf) 4 (Node Leaf 2 (Node Leaf 3 Leaf) )

tree02 :: BinaryTree Int 
tree02 = Node (Node Leaf 50 Leaf) 90 (Node Leaf 70 Leaf )

tree03 :: BinaryTree Int 
tree03 = Node tree01 16 tree02

tree04 :: BinaryTree Int 
tree04 = Node tree00 17 tree02

-- $
-- >>> perfect tree00
-- True
-- >>> perfect tree01
-- False
-- >>> perfect tree02
-- True
-- >>> perfect tree03
-- False
-- >>> perfect tree04
-- True
-- >>> perfect tree03 == (perfect' tree03)
-- True
-- >>> perfect tree04 == (perfect' tree04)
-- True