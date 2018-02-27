module AnaListReplicate where 

import Data.Functor.Foldable

-- | The replicate function as an anamorphism
anareplicate :: Int -> a -> [a] 
anareplicate n ch = anarep (ch,n)
  where
    anarep = ana coalg
    coalg ::  (a, Int) -> ListF a (a, Int)
    coalg (c, m) 
      | m <= 0  = Nil
      | otherwise = Cons c (c, m - 1)

-- $
-- >>> anareplicate 5 '*'
-- "*****"
-- >>> anareplicate 0 'X'
-- ""
-- >>> anareplicate (-10) 'X'
-- ""