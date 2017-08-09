-- Very convenient to use the Haskell extensions like `DeriveFunctor`,
-- `DeriveFoldable` and `DeriveTraversable` to autogenerate code.
--
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module TraversingLens where

import Control.Lens
import Data.Foldable 

data Pair a = Pair a a deriving (Show, Functor, Foldable, Traversable)
-- *TraversingLens Control.Lens Control.Lens> over traverse (+1) (Pair 3 4)
-- Pair 4 5

