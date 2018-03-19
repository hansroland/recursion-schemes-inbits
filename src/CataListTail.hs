module CataListTail where 

import Data.Functor.Foldable 

-- | The tail function as a catamorphism
--   Note, this is not a very efficient version
catatail :: [a] -> Maybe a 
catatail = cata alg where 
    alg :: ListF a (Maybe a) -> Maybe a 
    alg Nil = Nothing
    alg (Cons t  Nothing) = Just t 
    alg (Cons _  jt)  =  jt

-- $
-- >>> catatail [1,2,3,4]
-- Just 4