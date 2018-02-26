{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE DeriveFoldable #-} 
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module BinaryTree where 

import Data.Functor.Foldable.TH 

-- A simple BinaryTree with values in the Nodes
data BinaryTree a = Leaf 
                  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

makeBaseFunctor ''BinaryTree