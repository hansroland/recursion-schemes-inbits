{-# LANGUAGE ScopedTypeVariables #-}
module CataListFilter where 

import Data.Functor.Foldable 

-- | The filter function as a catamorphism
catafilter :: forall a. (a -> Bool) -> [a] -> [a] 
catafilter p = cata $ alg 
  where
    alg :: ListF a [a] -> [a]
    alg  Nil  =  []
    alg  (Cons x xs) = if (p x) then x : xs else xs

-- $    
-- >>>  catafilter odd [1..10]
-- [1,3,5,7,9]
-- >>> catafilter even []
-- []