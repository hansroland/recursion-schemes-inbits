module AnaTreeSubtrees where 

import Data.Functor.Foldable
import BinaryTree
    
-- | All subtrees of a tree using an anamorphism
--   Note: This enteres a lot of just leaves into the tree!!!!!!
--   See answer of Sassa NF in https://stackoverflow.com/questions/18520528/how-to-find-all-possible-subtrees-of-a-binary-tree-in-haskell
allSubtrees :: BinaryTree a -> [BinaryTree a]
allSubtrees tree = ana coalg [tree] where
    coalg [] = Nil
    -- coalg (Leaf : _) = Nil                                 -- this breaks example ex01
    coalg (Leaf : t ) = Cons Leaf t                           -- adds a lot of Leaf s to the
    coalg (n@(Node l _ r) : t) = Cons n (t ++ (l : [r]))      -- Here to not add the Leaf's

ex00 :: BinaryTree Char 
ex00 = Node (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf)) 'a' (Node Leaf 'e' Leaf)

ex01 :: BinaryTree Char
ex01 = Node (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf)) 'a' Leaf

-- $ 
-- >>> allSubtrees ex00
-- [Node (Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf)) 'a' (Node Leaf 'e' Leaf),Node (Node Leaf 'c' Leaf) 'b' (Node Leaf 'd' Leaf),Node Leaf 'e' Leaf,Node Leaf 'c' Leaf,Node Leaf 'd' Leaf,Leaf,Leaf,Leaf,Leaf,Leaf,Leaf]

-- To print out every subtree on a single line use:
-- mapM_ putStrLn (allSubtrees ex00)
-- compare result with src/AnaTreeSubtrees.hs    

-- Here we are building up a list of trees in the seed!!