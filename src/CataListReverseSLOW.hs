module CataListReverseSLOW where

-- | List reverse as catamorphism (direct implementation)
--      Note: Very slow implementation

import Data.Functor.Foldable
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