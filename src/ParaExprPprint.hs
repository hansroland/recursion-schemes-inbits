module ParaExprPprint where 

import Data.Functor.Foldable
import Expr
    
-- | Paramorphism: Print out simple expressions
pprint :: Expr ->  String
pprint = para alg 
  where
    alg :: ExprF (Expr, String) -> String
    alg (ValF n) = show n
    alg (AddF  (_, ls) (_, rs )) = ls ++ " + " ++ rs
    alg (MultF (l, ls) (r, rs )) = parens l ls ++ "*" ++ parens r rs
    --          |   |   |   +---- intermediate pprint result of the right multiplication factor 
    --          |   |   +-------- current expression of the right multiplication factor 
    --          |   +------------ intermediate pprint result of the left multiplication factor
    --          +---------------- current expression of the left multiplication factor

-- | parens: little helper function to enclose an expression in brackets
parens :: Expr -> String -> String 
parens (Add _ _) str = " (" ++ str ++ ") "
parens _ str = str

expr00 :: Expr 
expr00 = Mult (Add (Val 4) (Val 5)) (Add (Val 7) (Val 8))

expr01 :: Expr 
expr01 = Add (Mult (Val 4) (Val 5)) (Mult (Val 7) (Val 8))

-- $
-- >>> pprint expr00 
-- " (4 + 5) * (7 + 8) "
-- >>> pprint expr01 
-- "4*5 + 7*8"
