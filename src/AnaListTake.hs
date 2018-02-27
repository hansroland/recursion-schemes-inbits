module AnaListTake where 

import Data.Functor.Foldable

-- | The Prelude function 'take n' implemented as an anamorphism 
anatake :: Int -> [a] -> [a] 
anatake count xs = ana  coalg (xs, count) where 
    coalg ::  ([a], Int) -> ListF a ([a], Int )   
    coalg ([], _)     = Nil                  -- premature end of list
    coalg (y : ys, n) 
      | n <= 0        = Nil                  -- we have the specified amount of items
      | otherwise     = Cons y (ys, n - 1)   -- take one item, decrement counter

-- $
-- >>> anatake 10 [1..20]
-- [1,2,3,4,5,6,7,8,9,10]
-- >>> anatake 10 [1..5]
-- [1,2,3,4,5]
-- >>> anatake 0 [1..20]
-- []
-- >>> anatake (-2) [1..20]
-- []