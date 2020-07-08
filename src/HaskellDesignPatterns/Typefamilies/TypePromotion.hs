
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

-- A better way to understand type promotion is from the context of a
-- type-level programming example. For the coming example, i want to create a
-- list where the type itself contains information about the list size.
--
-- To represents numbers at type-level, we use the Peano numbering principles
-- which can be used to describe natural numbers (recall that this set of
-- numbers consists of integers greater than zero i.e. 0)
--

data Zero = Zero deriving Show
data Succ n = Succ n deriving Show

one :: Succ Zero
one = Succ Zero 
two :: Succ (Succ Zero)
two = Succ one

-- The above seems normal but it also allows other expressions which is also
-- legal.
ten = Succ 10 -- ten :: Succ Integer
rubbish = Succ True -- rubbish :: Succ Bool
-- Why does this happen? the type parameter 'n' in the definition of 'Succ' is
-- an arbitrary type which has no constraint and you can see this in the
-- definition of "ten" and "rubbish"
-- 
-- The larger question is that i wish to write a generic list or vector which
-- encapsulates natural numbers (i.e. i want to use Peano-encoding).
--
-- The 'Vec' data type has two type parameters: the first represents the list
-- data and the second represents the list size, that is either Zero or Succ
-- (this part is not enforced by the type-checker).
--

data Vec :: * -> * -> * where
  Nil :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)

nil = Nil :: Vec Int Zero

-- The following patterns build upon the previous i.e. pat3 -> pat2 -> pat1
-- where -> means "is built on" and this is kinda nice :)
pat1  = Cons 1 nil  :: Vec Int (Succ Zero)
pat2  = Cons 2 pat1 :: Vec Int (Succ (Succ Zero))
pat3  = Cons 3 pat2 :: Vec Int (Succ (Succ (Succ Zero)))

-- The following expressions are still legal but they don't build on one
-- another; this is not very nice ... i mean it doesn't exactly inspire
-- confidence in using Haskell
pat1_ = Cons 1 nil :: Vec Int (Succ Zero)
pat2_ = Cons 2 nil :: Vec Int (Succ Zero)
pat3_ = Cons 3 nil :: Vec Int (Succ Zero)


-- Here's another example of how Haskell permits expressions like this ... what
-- to do to make it safer? To be exact, we need type-safety when working with
-- "kinds".
badVec = Nil :: Vec Zero Zero



-- With the "DataKinds" extension enabled, this extension promots the Nat type
-- to the Nat kind. The data constructors ZeroD and SuccD are promoted to
-- types.

data Nat = ZeroD | SuccD Nat

-- badSucc = SuccD 10 -- compare this the definition of "ten" which is permissible but badSucc is not.


data VecD :: * -> Nat -> * where
  NilD  :: VecD a ZeroD
  ConsD :: a -> VecD a n -> VecD a (SuccD n)

-- Promoted types and kinds can be prefixed with a quote-character (i.e. ') to
-- unambiguously specify the promoted type or kind.

consD = ConsD 3 NilD :: VecD Integer ('SuccD 'ZeroD)

-- This still works (i.e. building upon previous constructions)as expected... before DataKinds and
-- after the use of DataKinds extension i.e pat1 -> pat2 -> pat3
patA = ConsD 1 NilD :: VecD Integer ('SuccD 'ZeroD)
patB = ConsD 2 patA :: VecD Integer ('SuccD ('SuccD 'ZeroD))
patC = ConsD 3 patB :: VecD Integer ('SuccD ('SuccD ('SuccD 'ZeroD)))

-- This compiles but still does not make sense; the problem here is not at the
-- type-level but rather it needs a particular recursion scheme technique to
-- de-construct or unfold the computation
pat1__ = ConsD 1 NilD :: VecD Integer ('SuccD 'ZeroD)
pat2__ = ConsD 2 NilD :: VecD Integer ('SuccD 'ZeroD)
pat3__ = ConsD 3 NilD :: VecD Integer ('SuccD 'ZeroD)

-- The following expression is no longer permissible because we have
-- constrained that the second type parameter should be a Nat and its been
-- promoted to a type (via DataKinds extension).
-- 
-- Uncomment the following to see why it fails to type-check :
-- badVecD = NilD :: VecD ZeroD ZeroD -- compare this with "badVec"

--------------------------

-- Add is a type-level function that adds two type-level numbers. We can
-- express this as a type family
--
type family Add (n :: Nat) (m :: Nat) :: Nat
type instance Add ZeroD m = m
type instance Add (SuccD n) m = SuccD (Add n m)

append :: VecD e n -> VecD e m -> VecD e (Add n m)
append NilD          l = l
append (ConsD x xs) ys = ConsD x (append xs ys)

items :: VecD Integer ('SuccD ('SuccD ('SuccD 'ZeroD)))
items = append (ConsD 1 (ConsD 2 NilD)) (ConsD 3 NilD)

