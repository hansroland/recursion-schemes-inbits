module AnaListSlides where 

import Data.Functor.Foldable
    
-- | Sliding windows with an anamorphism
anaslide :: Int -> [a] -> [[a]]
anaslide size = ana coalg where
    coalg :: [a] -> ListF [a] [a]
    coalg [] = Nil
    coalg l@(_ : t) 
      | length l >= size = Cons (take size l) t 
      | otherwise        = Nil 

-- $
-- >>> anaslide 3 [1..6]
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
