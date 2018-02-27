module AnaListTails where 

import Data.Functor.Foldable
    
-- | All tails of a list using an anamorphism
--   Note, this does not add the empty list to the result
anatails :: [a] -> [[a]]
anatails = ana coalg where
    coalg :: [a] -> ListF [a] [a]
    coalg [] = Nil
    coalg l@(_ : t) = Cons l t 

-- $ 
-- >>> anatails [1,2,3,4]
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
-- >>> anatails []
-- []