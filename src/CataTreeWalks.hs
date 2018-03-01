module CataTreeWalks where 

import Data.Functor.Foldable
import BinaryTree

-- | Flatten a binary tree with a catamophism using a preorder tree walk
--   Preorder: Visit first the node, then the subtrees
preorder :: BinaryTree a -> [a]
preorder = cata alg where 
    alg :: (BinaryTreeF a [a]) -> [a]
    alg (NodeF l v r) = v : l ++ r 
    alg LeafF         = []

-- | Flatten a binary tree with a catamorphism using a postorder tree walk
--   Postorder: Visit first the left and right subtrees, then the node value
postorder :: BinaryTree a -> [a]
postorder = cata alg where 
    alg :: (BinaryTreeF a [a]) -> [a]
    alg (NodeF l v r) = l ++ r ++ [v] 
    alg LeafF         = []

-- | Flatten a binary tree with a catamorphism using an inorder tree wal
--   Inorder: Visit first the left subtree, then the node value, and then the right subtree
inorder :: BinaryTree a -> [a]
inorder = cata alg where 
    alg :: (BinaryTreeF a [a]) -> [a]
    alg (NodeF l v r) = l ++ [v] ++ r 
    alg LeafF         = []

tree :: BinaryTree Int 
tree = Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf)) 1 (Node (Node Leaf 6 Leaf) 5 (Node Leaf 7 Leaf))

-- $
-- >>> preorder tree
-- [1,2,3,4,5,6,7]
-- >>> postorder tree
-- [3,4,2,6,7,5,1]
-- >>> inorder tree
-- [3,2,4,1,6,5,7]
-- >>> preorder Leaf
-- [] 
