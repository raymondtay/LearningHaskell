{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- 
-- LambdaCase: 
--  Anytime you would have written `\x -> case of ...`, you can replace with
--  `\case -> ...` and does not bind the 'x' to a name.
--

import Data.Functor.Foldable
import Data.List.Ordered (merge)
import Prelude hiding (Foldable, succ)

data NatF a = ZeroF | SuccF a deriving (Show, Functor)
type Nat = Fix NatF -- Declaring that `Nat` is a type alias to a 'recursive defn of NatF'

zero :: Nat 
zero = Fix ZeroF

succ :: Nat -> Nat
succ = Fix . SuccF

natsum :: Nat -> Int
natsum = cata alg where
  alg ZeroF     = 0
  alg (SuccF n) = n + 1

-- `natsum (succ (succ zero))` == 2 (OK)
--

-- 
-- Here's another way to write the same function as above 'natsum' by
-- leveraging on 'LambdaCase'
--
natsum' :: Nat -> Int
natsum' = cata $ \case
  ZeroF   -> 0
  SuccF n -> n + 1

