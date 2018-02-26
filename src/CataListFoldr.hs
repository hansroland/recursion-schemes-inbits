{-# LANGUAGE ScopedTypeVariables #-}
module CataListFoldr where 

import Data.Functor.Foldable 

-- | Implement good old foldr as a catamorphism
catafoldr ::  forall a b. (a -> b -> b) -> b -> [a] -> b 
catafoldr f b = cata alg where
  alg :: ListF a b -> b
  alg Nil  =  b
  alg (Cons a x) = f a x

-- $  
-- >>> catafoldr (+) 0 [1..10]
-- 55
-- >>> catafoldr (*) 1 [1..10]
-- 3628800
-- >>> catafoldr (*) 1 [1..100] == (foldr (*) 1 [1..100])
-- True
-- >>>  catafoldr (+) 0 []
-- 0