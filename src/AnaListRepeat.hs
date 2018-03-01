module AnaListRepeat where 

import Data.Functor.Foldable
    
anarepeat :: a -> [a] 
anarepeat = ana coalg where
    coalg :: a -> ListF a a
    coalg x = Cons x x
    
-- $ 
-- >>> take 10 $ anarepeat 5
-- [5,5,5,5,5,5,5,5,5,5]