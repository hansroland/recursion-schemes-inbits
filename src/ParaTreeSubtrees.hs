module ParaTreeSubtrees where 

import Data.Functor.Foldable
import BinaryTree

-- | All subtrees of a tree using a paramorphism
allSubtrees :: BinaryTree a -> [BinaryTree a]     
allSubtrees = para alg where
    alg :: BinaryTreeF a (BinaryTree a, [BinaryTree a]) -> [BinaryTree a]    
    alg LeafF = []
    alg (NodeF (l, ll) v (r, rr)) = ll ++ rr ++ [Node r v l]

ex00 :: BinaryTree Char 
ex00 = Node (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf)) 'a' (Node Leaf 'e' Leaf)

ex01 :: BinaryTree Char 
ex01 = Node (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf)) 'a' Leaf

-- $
-- >>> allSubtrees ex00
-- [Node Leaf 'c' Leaf,Node Leaf 'd' Leaf,Node (Node Leaf 'd' Leaf) 'b' (Node Leaf 'c' Leaf),Node Leaf 'e' Leaf,Node (Node Leaf 'e' Leaf) 'a' (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf))]


-- To print out every subtree on a single line use:
-- mapM_ (putStrLn . show) (allSubtrees ex00)

-- compare result with src/AnaTreeSubtrees.hs