module Semigroup where

{-
 - To get from Monoid to a Semigroup, we simply no longer
 - furnish nor require an identity.
 - The core operation remains binary and associative. Semigroup
 - still provides a binary associative operation, one that 
 - typically joins two things together (as in concatenation or summation)
 - but doesn't have an identity value. In that sense, it's a weaker algebra.
 -
 - Not yet part of base.
 -}

class Semigroup a where
  (<>) :: a -> a -> a


-- here, :| is an infix data constructor that takes two (type) arguments. 
-- It's a product of a and [a]. It guarantees that we always have at least one value
-- of type a, which [a] does not guarantee as any list might be empty.
-- Note that although :| is not alphanumeric, as most of the other data constructors
-- you are using to seeing are, it is jut a name for an infx data constructor. 
-- Data constructors with only non-alphanumeric sumbols and that being with a colon
-- are infix by default; those with alphanumeric names are prefix by default:
--
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)

data Trivial = Trivial

instance Semigroup Trivial where
  (<>) = undefined

trivialSmush :: Trivial -> Trivial -> Trivial 
trivialSmush = (<>)



























