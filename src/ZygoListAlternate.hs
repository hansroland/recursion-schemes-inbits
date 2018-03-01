module ZygoListAlternate where 

import Data.Functor.Foldable 

-- | Example of a zygomorphism. Insert lternatively (+) and (-) operators 
--   between the elements of a list.
--   See: https://github.com/haroldcarr/presentations/blob/master/2017-05-27-lambdaconf-recursion-schemes.pdf
--        Zygomorphism example
altern :: [Int] -> [Int]
altern = zygo f1 f2  
  where
    f1 :: ListF Int Bool -> Bool
    f1 Nil = False
    f1 (Cons _ b) = not b
    f2 :: ListF Int (Bool, [Int]) -> [Int]
    f2 Nil = []
    f2 (Cons x (b,xs)) = (sgn b x) : xs 
    sgn False x = x 
    sgn True  x = -x

-- Note 
-- f1 sees its own result from the previous iteration
-- f2 sees its own result and the result of f1 from the previous iteration

-- $    
-- >>> altern [1,2,3,4,5,6,7]
-- [1,-2,3,-4,5,-6,7]
-- >>> altern [1,2,3,4,5,6]
-- [-1,2,-3,4,-5,6]
-- >>> altern []
-- []