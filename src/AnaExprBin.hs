module AnaExprBin where 

import Data.Functor.Foldable 
import Expr 

-- Build an Add expression from a list of Ints
-- Note: Does not work with the empty list. 
--       The real problem is, that our Expr type cannot express empty expressions 
-- See: 
anaBuildExpr :: [String] -> Expr 
anaBuildExpr = ana coalg where 
    coalg :: [String] -> ExprF [String]
    coalg [n] = ValF (read n)
    coalg ns  = AddF left right 
      where
        (left, right) = splitAt (lh ns) ns
        lh :: [a] -> Int
        lh = (`div` 2) . length

-- $
-- >>> anaBuildExpr ["1", "2", "3", "4"]
-- Add (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))
-- >>> anaBuildExpr ["1", "2", "3", "4", "5"]
-- Add (Add (Val 1) (Val 2)) (Add (Val 3) (Add (Val 4) (Val 5)))