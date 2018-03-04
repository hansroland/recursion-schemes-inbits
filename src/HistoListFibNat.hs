module HistoListFibNat where

import Data.Functor.Foldable
import Control.Comonad.Cofree
import Numeric.Natural
    
-- | Fibonnacci function as a histomorphism using Natural (Peano) numbers
histofib :: Natural -> Integer
histofib n = histo alg (n - 1) where
    alg :: Maybe (Cofree (Maybe) Integer) -> Integer 
    alg (Just  (x :< (Just (y :< _)))) = x + y
    alg (Just  (_ :< _))               = 1
    alg _                              = 0
   
-- $
-- >>> histofib 10
-- 34 
-- >>> histofib 100
-- 218922995834555169026

-- :kind! Base Natural 
-- = Maybe


-- Maybe's as Peano numbers 
-- 0 ~ Nothing
-- 1 ~ Just Nothing 
-- 2 ~ Just (Just Nothing)
-- 3 ~ Just (Just (Just Nothing))
-- 4 ~ Just (Just (Just (Just Nothing)))


-- :t Just (Just Nothing)
-- Just (Just Nothing) :: Maybe (Maybe (Maybe a))