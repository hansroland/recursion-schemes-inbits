module HyloExprBin where 

import Data.Functor.Foldable
import Expr 

-- | Convert a normal integer to some kind of binary representation
--   See: https://idontgetoutmuch.wordpress.com/2011/11/12/anamorphism-example/ 
toBin :: Int -> String 
toBin = hylo alg coalg where
    alg :: ExprF String-> String
    alg (ValF x) = show x
    alg (AddF x y) = "(" ++ x ++ "+" ++ y ++ ")"
    alg (MultF x y) = "(" ++ x ++ "*" ++ y ++ ")"
    coalg :: Int -> ExprF Int 
    coalg (-1)          = ValF (-1)
    coalg 0             = ValF 0 
    coalg 1             = ValF 1 
    coalg 2             = ValF 2 
    coalg n | n < 0     = MultF (-1) (abs n)
    coalg n | even n    = MultF 2 (n `div` 2)
    coalg n | otherwise = AddF 1 (n-1)

-- $
-- >>> toBin 16
-- "(2*(2*(2*2)))"
-- >>> toBin 15
-- "(1+(2*(1+(2*(1+2)))))"
-- >>> toBin 0 
-- "0"
-- >>> toBin (-7)
-- "(-1*(1+(2*(1+2))))"