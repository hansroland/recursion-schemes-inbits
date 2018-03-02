{-# LANGUAGE ScopedTypeVariables #-}
module HyloTreeQuicksort where 

import Data.Functor.Foldable 
import BinaryTree 

-- | Sort a list with a hylomorphism
hyloQSort :: forall a. Ord a => Ord a => [a] -> [a]
hyloQSort = hylo alg coalg where
    alg :: (BinaryTreeF a [a]) -> [a]
    alg (NodeF l v r) = l ++ [v] ++ r 
    alg LeafF         = [] 
    coalg :: [a] -> BinaryTreeF a [a]
    coalg []          = LeafF
    coalg [x]         = NodeF [] x []
    coalg (x : xs)    = NodeF (filter (<= x) xs) x (filter (> x) xs)

-- $
-- >>> hyloQSort [1,6,3,10,10,9,4,3,5]
-- [1,3,3,4,5,6,9,10,10]
