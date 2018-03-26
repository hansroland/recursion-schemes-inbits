module AnaListTails where 

import Data.Functor.Foldable
import Data.List (tails)                      -- just for our test
 
-- | All tails of a list using an anamorphism
--   Note, this does not add the empty list [] to the result
--   So it's NOT equivalent to the tails function of Data.List
--   See src/ApoListTails.hs
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
-- >>> anatails [1,2,3,4] == (tails [1,2,3,4])
-- False
-- >>> anatails [] == (tails [])
-- False