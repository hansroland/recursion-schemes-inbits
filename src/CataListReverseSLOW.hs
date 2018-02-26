module CataListReverseSLOW where

import Data.Functor.Foldable

-- | List reverse as catamorphism (direct implementation)
--      Note: Very inefficient and SLOW implementation
catarev :: [a] -> [a] 
catarev = cata alg where
    alg :: ListF a [a] -> [a]
    alg Nil = []
    alg (Cons x xs) = xs ++ [x]

-- $
-- catarev [1,20,300,4000] 
-- [4000,300,20,1] 
-- catarev [] 
-- []  