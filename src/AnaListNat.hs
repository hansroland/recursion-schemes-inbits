module AnaListNat where

import Data.Functor.Foldable 

-- | Generate the list of the natural numbers with an anamorphism
anaNat :: [Int]
anaNat = ana coalg 0 where 
    coalg :: Int -> ListF Int Int 
    coalg n = Cons n $ succ n

-- $ 
-- >>> take 10 anaNat
-- [0,1,2,3,4,5,6,7,8,9]