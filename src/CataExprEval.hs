module CataExprEval where 

import Data.Functor.Foldable
import Expr

-- | A simple expression evaluator for our expression 
eval :: Expr -> Int 
eval = cata alg where
    alg :: ExprF Int -> Int 
    alg (ValF  n)   = n 
    alg (AddF  l r) = r + l 
    alg (MultF l r) = r * l

-- Examples
expr00 :: Expr 
expr00 = Mult (Add (Val 4) (Val 5)) (Add (Val 7) (Val 8))

expr01 :: Expr 
expr01 = Add (Mult (Val 4) (Val 5)) (Mult (Val 7) (Val 8))

-- $
-- >>> eval expr00
-- 135
-- >>> eval expr01
-- 76     