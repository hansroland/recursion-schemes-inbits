module ParaListTails where 

import Data.Functor.Foldable
import Data.List (tails)                      -- just for our test
  
-- | The tails
paratails :: [a] ->  [[a]]
paratails = para alg 
  where
    alg :: ListF a ([a], [[a]]) -> [[a]]
    --           |   |     +---------------- accumulated result till now 
    --           |   +---------------------- sublist of the already processed items
    --           +-------------------------- current item on input list
    alg (Cons x (hs, res)) = (x : hs) : res
    alg Nil = [[]]

-- $
-- >>> paratails [1,2,3,4]
-- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
-- >>> paratails []
-- [[]]
-- >>> paratails [1,2,3,4] == (tails [1,2,3,4])
-- True
-- >>> paratails [] == (tails [])
-- True