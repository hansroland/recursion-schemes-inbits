module CataListLength where

import Data.Functor.Foldable 

-- | Length function for lists as a catamorphism 
catalen :: [a] -> Int
catalen = cata alg where 
    alg :: ListF a Int -> Int
    alg Nil        = 0 
    alg (Cons _ r) = r + 1

-- $
-- >>> catalen [] 
-- 0 
-- >>> catalen [1,2,3,4,5]
-- 5 
-- >>> catalen "Hello Recursion Schemes"
-- 23
