module PostproListNat where

import Data.Functor.Foldable 

-- | Generate the sequence of the Natural numbers with a postpromorphism
postpronat :: [Int]
postpronat = postpro nattrans coalg 0 where 
    nattrans :: ListF Int b -> ListF Int b
    nattrans (Cons a b) = Cons (a + 1) b
    coalg :: Int -> ListF Int Int
    coalg n = Cons n n

-- $ 
-- >>> take 10 postpronat
-- [0,1,2,3,4,5,6,7,8,9]