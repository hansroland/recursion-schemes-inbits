module CataListSum01 where 
  
import Data.Functor.Foldable 

-- | Sum up a simple list of Ints
catasum :: [Int] -> Int 
catasum = cata alg where
    alg :: ListF Int Int -> Int
    alg Nil  = 0
    alg (Cons a r) = a + r

-- $
-- >>>  catasum [1,2,3,4]
-- 10
-- >>> catasum []
-- 0