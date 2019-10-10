
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

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
  

instance Applicative sig => Monad (Prog sig) where
  return = Return

  Return v >>= prog = prog v
  Op op >>= prog = Op (fmap (>>= prog) op)


