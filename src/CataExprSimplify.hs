module CataExprSimplify where 

import Data.Functor.Foldable
import Expr
    
-- | Chaining catamorphisms 

-- | Optimize: Remove addition with zero
optZero :: Expr -> Expr 
optZero = cata algOptZero

-- | Algebra to optimize addition with zero
algOptZero :: ExprF Expr -> Expr 
algOptZero (AddF (Val 0) r) = r 
algOptZero (AddF l (Val 0)) = l 
algOptZero x                = embed x

-- | Optimize: Remove multiplication with one
optOne :: Expr -> Expr 
optOne = cata algOptOne

-- | Optimizing F-Algebra to remove multiplication with zero and one 
algOptOne :: ExprF Expr -> Expr 
algOptOne (MultF (Val 1) r) = r
algOptOne (MultF l (Val 1)) = l 
algOptOne (MultF (Val 0) _) = Val 0
algOptOne (MultF _ (Val 0)) = Val 0 
algOptOne x                 = embed x

-- | Do both optimizations in one step
optimize :: Expr -> Expr 
optimize = cata $ algOptZero . project . algOptOne

-- Some examples
expr01 :: Expr 
expr01 = Mult (Val 1) (Val 10)

expr02 :: Expr 
expr02 = Mult (Val 9) (Val 1)

expr03 :: Expr 
expr03 = Mult (Val 1) (Val 1)

expr04 :: Expr 
expr04 = Mult (Val 5) (Val 5)

expr05 :: Expr 
expr05 = Add (Val 0) (Val 10)

expr06 :: Expr 
expr06 = Add (Val 7) (Val 0)

expr07 :: Expr 
expr07 = Add (Val 0) (Val 0)

expr08 :: Expr 
expr08 = Add (Val 5) (Val 5) 

expr09 :: Expr 
expr09 = Add (Add (Val 0) (Val 0)) (Mult (Val 1) (Val 1))


expr10 :: Expr 
expr10 = Mult (Add (Add (Val 4) (Val 5)) (Val 0)) (Add (Val 7) (Val 8))

expr11 :: Expr 
expr11 = Add (Mult (Mult (Val 14) (Val 15)) (Val 1)) (Mult (Val 17) (Val 18))

expr12 :: Expr
expr12 = Mult expr10 expr11

-- $
-- >>> optimize expr01
-- Val 10
-- >>> optimize expr02
-- Val 9
-- >>> optimize expr03
-- Val 1
-- >>> optimize expr04
-- Mult (Val 5) (Val 5)
-- >>> optimize expr05
-- Val 10
-- >>> optimize expr06
-- Val 7
-- >>> optimize expr07
-- Val 0
-- >>> optimize expr08
-- Add (Val 5) (Val 5)
-- >>> optimize expr09
-- Val 1
-- >>> optimize expr12
-- Mult (Mult (Add (Val 4) (Val 5)) (Add (Val 7) (Val 8))) (Add (Mult (Val 14) (Val 15)) (Mult (Val 17) (Val 18)))
-- >>> optimize expr12 == (optOne $ optZero expr12)
-- True