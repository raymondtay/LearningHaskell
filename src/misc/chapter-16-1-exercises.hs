module Chap16_1 where

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

-- 
-- I have bound (Two a) to the defn of Functor and hence 
-- the type parameter 'a' is no longer available; the next 
-- thing is to consider the type of fmap which is 
-- 
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- 
-- a crucial thing you need to understand is that names in 
-- signatures only have meaning in relationship to between 
-- themselves; which means 'f' is now (Two a) and so we can only consider
-- the type parameter 'b' which would now 'a'
--
-- Once you understand this, apply your understanding to
-- `Or a b` and see for yourself.
--
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)


-- b is the phantom type
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant c) = Constant c

-- when you fmap the const function over the Constant type, the
-- first argument to const is never used because the partially 
-- applied const is itself never used. The first type argument
-- to Constant's type constructor is in the part of the structure
-- that Functor skips over. 
-- The second argument to the Constant type constructor is the phantom
-- type variable b which has no value or term-level witness in the datatype.
-- Sienc there are no values of the type the Functor is supposed to 
-- be mapping, we have nothing we are allowed to apply the fmap'd 
-- function to, so we never the const expressions.

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- this is slightly more complicated....
-- we fallback to the strategy of following types.
-- :k Functor is * -> * -> Constraint
-- :k Wrap is (* -> *) -> * -> * 
-- so it makes sense to first write:
-- ```
-- Functor (Wrap f) 
-- ```
-- but then we must realize that "fmap" takes a function
-- and consumes a functor so it means "fa" here must be a functor.
-- so we write "Wrap (fmap f fa)" because Wrap must result in a Functor;
-- then the compiler would ring you to say that the Functor constraint must
-- be applied to "f".
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
























