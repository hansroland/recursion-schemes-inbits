module HyloListMovingAverage where 

import Data.Functor.Foldable

-- | Moving averages with an hylorphism
movavg :: Int -> [Double] -> [Double]
movavg size = hylo alg  coalg  where
    coalg :: [a] -> ListF [a] [a]
    coalg [] = Nil
    coalg l@(_ : t) 
      | length l >= size = Cons (take size l) t 
      | otherwise        = Nil  
    alg :: ListF [Double] [Double] -> [Double]
    alg Nil = [] 
    alg (Cons a r) = ((sum a)/fromIntegral size) : r

-- $
-- >>> movavg 4 [0,2,4,6,8,10,12,14,16]
-- [3.0,5.0,7.0,9.0,11.0,13.0]
-- >>> movavg 2 [0,2,4,6,8,10,12,14,16]
-- [1.0,3.0,5.0,7.0,9.0,11.0,13.0,15.0]
-- >>> movavg 4 [1,2,3]
-- []