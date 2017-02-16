{-# LANGUAGE InstanceSigs #-}

--
-- About Semigroups!!!
--
module Chapter15_4 where

import Control.Monad
import Data.Monoid
import Data.Semigroup

newtype Combine a b = Combine { unCombine :: (a -> b) }


