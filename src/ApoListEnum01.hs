module ApoListEnum01 where 

import Data.Functor.Foldable

-- | Enumeration with lower and upper bound
fromTo :: Int -> Int -> [Int] 
fromTo from to = apoenum from where
    apoenum = apo rcoalg 
    rcoalg :: Int -> ListF Int (Either [Int] Int)
    rcoalg n 
       | n >= to    = Cons n $ Left []
       --                  |    |    +-------- List that is appended to the result
       --                  |    +------------- Left signals: End of recursion
       --                  +------------------ Item added to the resulting list 
       --                                        (before the list in the Left is applied)
       | otherwise  = Cons n $ Right (n + 1)
       --                  |     |      + ---- Input item for the next recursion step
       --                  |     +------------ Right signals: Continue recursion
       --                  +------------------ Item added to the resulting list

-- $       
-- >>> fromTo 5 10 
-- [5,6,7,8,9,10]
-- >>> fromTo 10 5 
-- [10]