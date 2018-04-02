{-# LANGUAGE ScopedTypeVariables #-}
module ParaListDedup where 

import Data.Functor.Foldable

-- | Remove duplicates from a list 
dedup :: forall a.Eq a => [a] -> [a]
dedup = para alg where 
    alg :: ListF a ([a], [a]) -> [a]
    alg Nil                 = []
    alg (Cons x (inps, xs)) = if x `elem` inps then xs else x:xs

-- $
-- >>> dedup [1,2,3,4,5,6,5,4,3,2,1] 
-- [6,5,4,3,2,1]
    