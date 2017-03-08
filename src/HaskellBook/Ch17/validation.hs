{-# LANGUAGE InstanceSigs #-}

module Chapter_17_validation where

import Data.Semigroup
import Data.Monoid hiding ((<>))

--
-- The code here is implemented w.r.t page 749 of the haskellbook. let's go
-- thru the code step-by-step.
--
-- The end goal of the exercise is to implement an Applicative for the
-- Validation data type; which means that a Functor is needed. When i was
-- constructing the functor, i realize that i need to combine the values of the
-- failures and naturally i would want to leverage Monoids.
--
-- The problem why i shun Monoids in this scenario is because i dont know what
-- the default value for mempty should be w.r.t Validation datatype and hence i
-- use Semigroups since the real function i want to use is (<>).
--
-- After that comes the construction of the Applicative.
-- The thing about this Applicative is that i want to be able to propagate both
-- successes and failures to the user and the Semigroup code clearly defines
-- the ability to "merge" values of both data types.
--
data Validation err a = Failure err | Success a deriving (Eq, Show)

instance Functor (Validation err) where
  fmap f (Failure err) = Failure err
  fmap f (Success a) = Success (f a)

instance (Semigroup err, Semigroup a) => Semigroup (Validation err a) where
  (<>) (Failure err1) (Failure err2) = Failure ((<>) err1 err2)
  (<>) (Failure err1) (Success _) = Failure err1
  (<>) (Success _) (Failure err1) = Failure err1
  (<>) (Success a) (Success b) = Success ((<>) a b)
  
instance Applicative (Validation err) where
  pure :: a -> Validation err a
  pure a = Success a
  (<*>) :: Validation err (a -> b) -> Validation err a -> Validation err b
  (<*>) (Failure f) _           = Failure f
  (<*>) (Success f) (Failure e) = Failure e
  (<*>) (Success f) (Success b) = Success (f b)

data Errors =
  DividedByZero
    | StackOverflow
    | MooglesChewedWires
    deriving (Eq, Show)

