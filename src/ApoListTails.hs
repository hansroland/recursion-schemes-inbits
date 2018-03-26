module ApoListTails where 

import Data.Functor.Foldable
import Data.List (tails)                      -- just for our test
       
-- | All tails of a list using an apomorphism
--   Compare this with the file AnaListTails.hs 
--   This is equivalent to the tails function of Data.List
apotails :: [Int] -> [[Int]]
apotails = apo coalg where
    coalg :: [Int] -> ListF [Int] (Either [[Int]] [Int])
    coalg []        = Cons []  $ Left []
    coalg l@(_ : t) = Cons l   $ Right t 

-- $
-- >>> apotails [1,2,3,4]
-- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
-- >>> apotails []
-- [[]]
-- >>> apotails [1,2,3,4] == (tails [1,2,3,4])
-- True
-- >>> apotails [] == (tails [])
-- True
