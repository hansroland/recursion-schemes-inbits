module CataListReverse where

-- List reverse as catamorphism. Fast version using the ShowS trick.
-- See: https://stackoverflow.com/questions/9197913/what-is-the-shows-trick-in-haskell
     
import Data.Functor.Foldable

type List2List a = [a] -> [a]

catarev :: [a] -> [a] 
catarev as = cata alg as [] where
    alg :: ListF a (List2List a) -> List2List a
    alg Nil = id
    alg (Cons x r) = r . (x :)

-- $ 
-- catarev [1,20,300,4000] 
-- [4000,300,20,1]
-- catarev [] 
-- []