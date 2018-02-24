module CataListSum01 where 
  
-- Sum up a simple list of Ints

import Data.Functor.Foldable 

catasum :: [Int] -> Int 
catasum = cata alg 
  where
    alg :: ListF Int Int -> Int
    alg Nil  = 0
    alg (Cons a b) = a + b

-- $
-- >>>  catasum [1,2,3,4]
-- 10
-- >>> catasum []
-- 0