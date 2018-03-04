module HistoListFibInt where

import Data.Functor.Foldable
import Control.Comonad.Cofree

-- | Fibonnacci function as a histomorphism using a list of length n
histofib :: Int -> Integer
histofib n = histofibgo $ replicate (n - 1) '*'

histofibgo :: [Char] -> Integer
histofibgo = histo alg where
    alg :: ListF Char (Cofree (ListF Char) Integer) -> Integer 
    alg (Cons _ (x :< (Cons _ (y :< _)))) = x + y
    alg (Cons _ (_ :< _))                 = 1
    alg _                                 = 0

-- $
-- >>> histofib 10
-- 34 
-- >>> histofib 100
-- 218922995834555169026