{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE DeriveFoldable #-} 
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where 

import Data.Functor.Foldable.TH

-- | Asimple expression tree with addition and multiplication
data Expr = Add Expr Expr
    | Mult Expr Expr 
    | Val Int
    deriving(Eq, Show)

makeBaseFunctor ''Expr
