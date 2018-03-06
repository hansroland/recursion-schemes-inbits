module AnaListEnum where 

import Data.Functor.Foldable 

-- | Enumeration with lower and upper bound as an anamorphism
anaEnumfromTo :: Int -> Int -> [Int] 
anaEnumfromTo from to = anaenum from where
    anaenum = ana coalg 
    coalg :: Int -> ListF Int Int
    coalg n 
       | n > to = Nil
       | otherwise = Cons n (n + 1)

-- >>> anaEnumfromTo 4 10
-- [4,5,6,7,8,9,10]