module PreproListPoly where

import Data.Functor.Foldable 

-- use a prepromorphism to model polynomials
polyprepro :: [Integer] -> Integer 
polyprepro = prepro nattran alg where 
    nattran :: ListF Integer b -> ListF Integer b
    nattran (Cons n x) = Cons (10 * n) x
    nattran Nil = Nil
    alg :: ListF Integer Integer -> Integer 
    alg (Cons n s) = n + s 
    alg Nil = 0

-- $    
-- >>> polyprepro [3,4,5,6]
-- 6543