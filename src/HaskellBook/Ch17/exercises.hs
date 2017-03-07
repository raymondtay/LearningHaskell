{-# LANGUAGE InstanceSigs #-}

module Chapter_17 where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

-- 
-- (<*>) :: f ( a -> b ) -> f a -> f b
-- wih the constraint that f is a Functor
-- so the construction of the expression for (<*>) is like this:
-- (1) i know its a function embedded in a functor, in this case Identity
--     and so i use the expression (Identity f) which is the same as (Identity
--     (a -> b))
-- (2) next thing i did is to provide a functor instance for Identity and then
--     the rest is quite straightforward w.r.t the reasons i gave for Functor.
--     See the previous exercises if i'm still confused.
--
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)


-- 
-- This seems to be an example of a monoidal-functor.
--
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show) 

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance (Monoid a, Monoid b) => Monoid (Constant a b) where
  mempty = mempty
  mappend (Constant a) (Constant b) = Constant (a `mappend` b)

-- How did i arrive at this conclusion? Here's how.
-- 
-- 'pure' says that it returns a function like this : a -> f a
-- where 'f' is a Functor. By the previous definitions, i know that 'f' is both
-- a functor and monoid; so i apply 'a' (which i know to be monoid) to 'pure'
-- but i want the actual value embeded in the monoid context so i apply mempty
-- to 'a'.
--
-- Next, i look at (<*>) and then realize that 'a' is actually a function
-- embedded in the Constant context and since i know they are functors and
-- monoids, so i apply `mappend` to both .
instance Monoid a => Applicative (Constant a) where
  pure a = Constant (mempty a)
  (<*>) (Constant a) (Constant b) = Constant (mappend a b)

-- 
-- Copying from Haskell's inbuild data structure called `Maybe`
-- here is how i would declare a Maybe Applicative 
--
data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'
  (<*>) :: Maybe' ( a -> b ) -> Maybe' a -> Maybe' b
  (<*>) (Just' f) Nothing' = Nothing'
  (<*>) (Just' f) (Just' a) = Just' (f a)
  (<*>) (Nothing') _  = Nothing'

--
-- You normally would use an Applicative or craft of your own in the situation
-- where you are thinking : " I want to do something kinda like fmap, but my
-- function is embedded in the functorial structure too, not just the value i
-- want to apply my function to". This is the basic motivation of the
-- Applicative.
--
