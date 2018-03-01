module HyloListFact where 

import Data.Functor.Foldable

-- | Factorial as a Hylomorphism: a Coalgebra creates a list from n to 1
--                               an Algebra multiplies everything up 
hylofact :: Int -> Int
hylofact = hylo alg coalg
  where 
    coalg :: Int -> ListF Int Int
    coalg m 
       | m <= 1 = Nil
       | otherwise = Cons m (m - 1)
    alg :: ListF Int Int -> Int
    alg Nil  =  1
    alg (Cons a x) = a * x

-- $
-- >>> hylofact 5
-- 120
-- >>> hylofact 0 
-- 1