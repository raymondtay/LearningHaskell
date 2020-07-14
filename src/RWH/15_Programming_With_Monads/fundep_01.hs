{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses #-}

import Prelude hiding (even, odd) -- hide the prelude's definitions

class Extract container elem | container -> elem where
  extract :: container -> elem

--instance Extract (a, b) b where
  --extract (x, y) = y

instance Extract (a, b) a where
  extract (x, _) = x

-- some notes in order here...
-- (a, b) is the type of the `container` and the rhs is the type of the value
-- in both instances defined its 'a' and 'b' respectively.
--

{-

data Nat = Zero | Succ Nat
three = Succ (Succ (Succ Zero)) -- the number 3


even Zero = True
even (Succ n) = odd n
odd Zero = False
odd (Succ n) = even n

-}

data Nat = Zero | Succ Nat
--data Succ n 

--type Three = Succ (Succ (Succ Zero))

--class Even n
--class Odd n
--instance Even Zero
--instance Even (Succ n)
--instance Odd (Succ n)

class Even n b | n -> b where
  even :: n -> b
class Odd n b | n -> b where
  odd :: n -> b

-- this relation says that the relation Even n b is actually a function from n
-- to b.This prevetns us from at the same time declaring both Even Zero True
-- and Even Zero False and allows b to be computed if n is a known number
--

instance Even a b where
  even Zero = True


