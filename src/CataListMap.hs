{-# LANGUAGE ScopedTypeVariables #-}
module CataListMap where

import Data.Functor.Foldable 

-- | The map function implemented as a catamorphism
catamap :: forall a b.(a -> b) -> [a] -> [b]
catamap f = cata alg where 
    alg :: ListF a [b] -> [b]
    alg Nil = [] 
    alg (Cons a r) = (f a) : r

-- $ 
-- >>> catamap (+1) [1,2,3,4,5]
-- [2,3,4,5,6]