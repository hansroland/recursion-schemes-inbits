module PreproTreeDemo where 

import Data.Functor.Foldable 
import BinaryTree

-- | An example of a prepromorphism with a binary tree
preprotree :: BinaryTree Int -> BinaryTree Int 
preprotree = prepro nattrans alg where 
    nattrans :: BinaryTreeF Int b -> BinaryTreeF Int b
    nattrans LeafF = LeafF 
    nattrans (NodeF l n r) = NodeF l (n*n) r
    alg :: BinaryTreeF a (BinaryTree a)  -> BinaryTree a
    alg LeafF = Leaf 
    alg (NodeF l n r) = Node l n r

preptree :: BinaryTree Int
preptree = Node (Node Leaf 7 Leaf) 3 (Node Leaf 5 (Node Leaf 11 Leaf))

-- $
-- >>> preprotree  preptree
-- Node (Node Leaf 49 Leaf) 3 (Node Leaf 25 (Node Leaf 14641 Leaf))