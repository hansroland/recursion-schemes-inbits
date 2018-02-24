module CataListSum02 where 

-- Given a list of stings representing numbers.
-- Sum these numbers.

import Data.Functor.Foldable 

catasum :: [String] -> Double 
catasum = cata alg where
    alg :: ListF String Double -> Double
    --              |     |         +---- type of result
    --              |     +-------------- type of result
    --              +-------------------- type of list item
    alg Nil  = 0.0
    alg (Cons x d) = read x + d
    --        | +--------------- intermediate result of the catasum function so far. Type Double
    --        +----------------- current list element (Type: String) 

-- $    
-- >>> catasum ["1.1", "2.2", "3.3"]
-- 6.6
-- >>> catasum []
-- 0.0