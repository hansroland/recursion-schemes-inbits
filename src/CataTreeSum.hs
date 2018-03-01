module CataTreeSum where 

import Data.Functor.Foldable
import BinaryTree

-- | Sum up the values in a tree
catasum :: BinaryTree String -> Int 
catasum = cata alg where 
    alg :: BinaryTreeF String Int -> Int
    alg LeafF = 0 
    alg (NodeF l v r) = l + (read v) + r

tree :: BinaryTree String 
tree = Node (Node Leaf "1" Leaf) "4" (Node Leaf "2" (Node Leaf "3" Leaf))

-- $
-- >>> catasum tree
-- 10

