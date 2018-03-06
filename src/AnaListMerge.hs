module AnaListMerge where 

import Data.Functor.Foldable 

-- | Merging 2 lists (example of Harold Carr)
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists as bs = ana coalg (as, bs) where 
    coalg :: Ord a => ([a], [a]) -> ListF a ([a], [a]) 
    coalg ([], [])      = Nil
    coalg ([], y:ys)    = Cons y ([], ys)
    coalg (x:xs, [])    = Cons x (xs, [])
    coalg (x:xs, y : ys) 
       | x < y          = Cons x (xs, y:ys)
       | otherwise      = Cons y (x:xs, ys)

-- >>> mergeLists [2,3,5] [1,4]
-- [1,2,3,4,5]