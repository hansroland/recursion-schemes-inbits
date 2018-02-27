module AnaListCollatz where 

import Data.Functor.Foldable

-- | The Collatz function as an anamorphism 
anacollatz :: Integer -> [Integer]
anacollatz = ana coalg where 
   coalg :: Integer -> ListF Integer Integer
   coalg n 
     | n <= 1          = Nil 
     --                   +-----------------------  Nil will stop generating the list  
     | n `mod` 2 == 0  = Cons n (n `div` 2) 
     --                       | (         ) ------  Seed (=input) for next recursion step
     --                       +-------------------- Item added to the resulting list
     | otherwise       = Cons n (3 * n + 1)

-- $
-- >>> anacollatz 3
-- [3,10,5,16,8,4,2]
-- >>> anacollatz 9
-- [9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2]
-- >>> anacollatz 0
-- []
    