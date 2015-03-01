{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{- was going thru Edward's presentation online at the following adddress https://www.youtube.com/watch?v=hIZxTQP1ifo -}

import GHC.Prim

class Category k where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id x = x
  f . g = \x -> f (g x)

data Dict (p:: Constraint) where
  D :: p => Dict p

newtype p :- q = Sub (p => Dict q)

(\\) :: p => (q => r) -> (p :- q) -> r
r \\ Sub D = r

instance Category (:-) where
  id  = Sub D
  p . q = Sub $ D \\ p \\ q
