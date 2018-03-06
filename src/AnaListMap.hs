{-# LANGUAGE ScopedTypeVariables #-}
module AnaListMap where 

import Data.Functor.Foldable
    
-- | The Prelude function 'map' implemented as an anamorphism 
anamap :: forall a b.(a -> b) -> [a] -> [b] 
anamap f = ana coalg  where 
    coalg :: [a] -> ListF b [a]
    coalg []       = Nil 
    coalg (x : xs) = Cons (f x) xs

-- $ 
-- >>> anamap (+1) [1,2,3,4,5]
-- [2,3,4,5,6]
-- >>> anamap (+1) []
-- []
    