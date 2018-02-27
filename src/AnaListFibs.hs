module AnaListFibs where 

import Data.Functor.Foldable

-- | The list of fibonacci numbers generated with an Anamorphism
anafibs :: [Integer] 
anafibs = (ana coalg) (0,1) where
    coalg :: (Integer, Integer) -> ListF Integer (Integer, Integer)
    coalg (n, m) = Cons n (m, m + n)

-- $
-- >>> take 10 $ anafibs
-- [0,1,1,2,3,5,8,13,21,34] 
-- >>> last $ take 100 $ anafibs
-- 218922995834555169026
