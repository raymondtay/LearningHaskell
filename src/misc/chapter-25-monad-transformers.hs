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
-- Referencing the type of `pure` which is `pure :: Applicative f => a -> f a`
-- the clue seems to be hidden within that defn.
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
-- By writing the type signature as part of the function
-- defn might help to unravel the mystery of how to write 
-- this particular instance. E.g. the type of 'a'
-- in the following definition is actually `f (g a)`
-- and type of 'f' is actually `f(g (a -> b))`
-- but how does this actually help me in figuring things out??
--
-- If we attempt to write an expression, the GHCI would throw the
-- following error:
--
-- *StartWithMonadT> Compose (Just (Just (+1))) <*> Compose (Just (Just 3))
-- Compose {getCompose = *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
--     undefined, called at chapter-25-monad-transformers.hs:107:33 in main:StartWithMonadT
--
-- After some R&D, the answer lies on this page => https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Data-Functor-Compose.html
-- 
-- 
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)


{-
 - What about Monad? There's no problem composing two arbitrary datatypes
 - that have Monad instances. We saw this already when we used Compose with
 - Maybe and list, which both have Monad instances defined. However, the 
 - result of having done so does not give you a Monad.
 -
 - The issue comes down to a lack of information. Both types Compose is working
 - with are polymorphic, so when you try to write a bind for the Monad
 - you are trying to combine two polymorphic binds into a single
 - combined bind. This, as it turns out, is not possible.
 -
 - Since getting another Monad given the compositino of two 
 - arbitrary types that have aMonad instance is impossible, what can we do to get
 - a Monad instance for combination of types?? The answer is, monad transformers.
 -
-}

instance (Foldable f, Foldable g) => 
  Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga


instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga


