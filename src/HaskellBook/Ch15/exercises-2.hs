{-# LANGUAGE InstanceSigs #-}

module Chapter15_2 where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S

data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance M.Monoid Trivial where
  mappend Trivial Trivial = Trivial
  mempty = Trivial

--
-- 4
--
newtype BoolConj = BoolConj Bool deriving Show
instance Monoid BoolConj where
  mempty = BoolConj False
  mappend (BoolConj False) (BoolConj True) = BoolConj True
  mappend (BoolConj False) _ = BoolConj False
  mappend (BoolConj True) _ = BoolConj True 

--
-- 8 
--

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x)) -- the type signature hinted at `a` being a `Monoid`
  --mappend = undefined
  mappend (Mem f) (Mem g) = Mem (\s -> 
    let (a1, s1) = (f s)
        (a2, s2) = (g s)     
    in (a2, s))


