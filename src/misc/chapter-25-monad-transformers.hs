{-# LANGUAGE InstanceSigs #-}

module StartWithMonadT where

{-
 - We have one bit of structure wrapped around another, then a value
 - type (the `a`) because the whole thing still has to be kind `*` in 
 - the end. We've made the point in previous chapters that type-
 - constructors are functions. Type constructors can take other type 
 - constructors as arguments, too, just as functions can take other 
 - functions as arguments. This is what allows us to compose types.
 -
 - To convince you, let's assume that you have the `Compose` loaded into GHCI
 - and then you enter `:t Compose Just` into GHCI and look at the output
 - which should give you the following:
 - *StartWithMonadT> :t Compose Just
 - Compose Just :: Compose ((->) a) Maybe a
 -
 -}
newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

--
-- We need to apply `fmap` twice to get to the 
-- value `inside`. Why? The answer is to work through 
-- the types ... and here's how:
-- when i say `fmap f` i'm applying "f" to the functor 
-- and this works when i say `fmap (+1) (Just 4)` but when
-- (Just 4) is embedded in another functor? e.g. [Just 4] the question
-- is how do we get to the Maybe inside []????
-- Answer is to apply fmap twice. i.e. `(fmap . fmap $ (+1)) [Just 4]`
-- and you get the answer `[Just 5]` and you would noticed that the outer-structure
-- [] wasn't "touched".
--
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


newtype One f a = One (f a) deriving (Eq, Show)

--
-- Note to myself: what the first line is really saying is
-- that (One f) is a Functor where f is also a functor
-- 
instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- and with this defn, we can do something like this:
-- #$> fmap (+2) (One (Just 3)) because Maybe is a Functor
--

newtype Three f g h a = Three (f (g (h a))) deriving (Eq,Show)
instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- with the above defn, we can write the following (because Maybe ∈ Functor):
-- #$> fmap (+2) $ Three (Just (Just (Just 3)))
-- #$> Three (Just (Just (Just 5)))
--
-- The way to think about this is that the composition of two datatypes
-- that have a Functor instance gives rise to a new Functor instance. 
-- You will sometimes see people refer to this as Functors being "closed 
-- under composition" which just means that when you compose two Functors,
-- you get another Functor.
--

-- 
-- Referencing the file `libraries/base/Control/Applicative.hs`
-- in the GHC codebase, i can see that the problem i have was how to
-- bring `f::pure` , `g::pure` into `Compose f g::pure`
-- and the trick is to leverage function composition, as shown below:
--
instance (Applicative f, Applicative g) => 
          Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

-- can i do the same thing for defining the `apply`
-- of Applicatives? we'll see soon.
--
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = undefined



{-
-- Can this be an Applicative ??? Doesn't look like it.
instance Functor f => Applicative (One f) where
  pure a = One $ Just a
  -- (<*>)  = undefined
-}
