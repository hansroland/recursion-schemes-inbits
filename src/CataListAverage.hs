module CataListAverage where 

-- Algebras over the same functor but different carrier types
-- can be combined as pairs. With this we can replace multiple 
-- catamorphisms by a single one. 

import Data.Functor.Foldable.Helpers

algCount :: ListF a Int -> Int
algCount Nil = 0 
algCount (Cons _ n) = n + 1

algSum :: ListF Double Double -> Double 
algSum Nil = 0
algSum (Cons a s) = a + s

-- | An average function based on a catamorphism.
--   Note this works only for non-empty lists.
--   Check division on zero according to your standards.
avg ::  [Double] -> Double 
avg xs =  ss / realToFrac count
    where
      (ss, count) = cata (zipAlgebras algSum algCount) xs

-- $
-- >>> avg [10, 190, 100]
-- 100.0
