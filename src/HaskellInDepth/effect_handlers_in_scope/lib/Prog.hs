
{-# LANGUAGE DeriveFunctor,
             InstanceSigs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module Prog where

-- We can generalize away from backtrackable computations by defining a
-- datatype that is parametric in the signature of the syntax.
-- We factor syntax for programs into the "Return" contructor and a constructor
-- "Op" that allows us to incorporate operations of interest from some signature
-- "sig".
data Prog sig a = Return a | Op (sig (Prog sig a)) deriving Functor

data Nondet cnt = Fail' | cnt :| cnt deriving Functor


instance (Applicative sig) => Applicative (Prog sig) where
  pure :: a -> Prog sig a
  pure = Return
  
  (<*>) :: Prog sig (a -> b) -> Prog sig a -> Prog sig b
  (<*>) (Return f) (Return a) = Return (f a)
  (<*>) (Op op) b = Op $ fmap (\innerOp -> innerOp <*> b) op

instance (Applicative sig) => Monad (Prog sig) where
  return = Return

  (Return v) >>= prog = prog v
  Op op >>= prog = Op(fmap (>>= prog) op)
  



-- Generally speaking, we will be dealing with several different effects that
-- work together in a single program, and so we need a means of flexibly
-- composing signatures, where each signature captures syntax that encodes a
-- particular effect.
--
data (sig1 + sig2) cnt = Inl (sig1 cnt) | Inr (sig2 cnt) deriving Functor

class (Functor sub, Functor sup) => sub ⊂ sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor sig => sig ⊂ sig where
  inj = id
  prj = Just

instance (Functor sig1, Functor sig2) => sig1 ⊂ (sig1 + sig2) where
  inj = Inl
  prj (Inl fa) = Just fa
  prj _ = Nothing

instance (Functor sig1, sig ⊂ sig2) => sig ⊂ (sig1 + sig2) where
  inj = Inr . inj
  prj (Inr ga) = prj ga
  prj _ = Nothing

inject :: (sub ⊂ sup) => sub (Prog sup a ) -> Prog sup a
inject = Op . inj

project :: (sub ⊂ sup) => Prog sup a -> Maybe (sub (Prog sup a))
project (Op s) = prj s
project _ = Nothing


