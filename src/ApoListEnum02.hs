module ApoListEnum02 where 

import Data.Functor.Foldable
    
-- | Enumeration with lower and upper bound
fromTo :: Int -> Int -> [Int] 
fromTo from to = apoenum from
  where
    apoenum = apo rcoalg 
    rcoalg :: Int -> ListF Int (Either [Int] Int)
    rcoalg n 
       | n >= to    = Cons n $ Left [100, 200, 300]
       | otherwise  = Cons n $ Right (n + 1)

-- $
-- >>> fromTo 5 10 
-- [5,6,7,8,9,10,100,200,300]
-- >>> fromTo 10 5 
-- [10,100,200,300]