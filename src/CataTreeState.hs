module CataTreeState where 

import Data.Functor.Foldable.Exotic 
import Control.Monad.State
import BinaryTree

catanum :: BinaryTree a -> BinaryTree (a, Int)
catanum tr = evalState (catanum' tr) [1..] 

catanum' :: BinaryTree a -> State [Int] (BinaryTree(a, Int))
catanum' = cataM alg where
    alg :: BinaryTreeF a (BinaryTree(a, Int)) -> State [Int] (BinaryTree(a, Int))
    alg LeafF = return Leaf
    alg (NodeF l v r) = do 
        n <- pop
        return $ Node l (v,n) r

pop :: State [Int] Int 
pop = do 
    (y :ys) <- get
    put ys
    return y

tree :: BinaryTree Char
tree = Node (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf)) 'd' (Node (Node Leaf 'e' Leaf) 'f' (Node Leaf 'g' Leaf))

-- $ 
-- >>> catanum tree
-- Node (Node (Node Leaf ('a',1) Leaf) ('b',3) (Node Leaf ('c',2) Leaf)) ('d',7) (Node (Node Leaf ('e',4) Leaf) ('f',6) (Node Leaf ('g',5) Leaf))