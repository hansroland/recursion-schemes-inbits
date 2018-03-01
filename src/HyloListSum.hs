module HyloListSum where 

import Data.Functor.Foldable 

hylosum :: Integer -> Integer
hylosum end = hylo alg coalg 1
  where 
    coalg :: Integer -> ListF Integer Integer
    coalg m 
       | m > end = Nil
       | otherwise = Cons m (m + 1)
    alg :: ListF Integer Integer -> Integer
    alg Nil  =  0
    alg (Cons a x) = a + x
