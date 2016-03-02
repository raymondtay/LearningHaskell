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


