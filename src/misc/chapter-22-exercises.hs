{-# LANGUAGE InstanceSigs #-}

module Chap22 where

import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

--
-- The reader newtype has a handy runReader accessor to get the function
-- out of Reader. Let us prove for ourselves that this is the 
-- same thing, but with a touch of data constructor jiggery-pokery
-- mixed in. What does the Functor for this look like compared to 
-- function composition?
--
-- instance Functor (Reader r) where
--   fmap f (Reader ra) = Reader $ \r -> f (ra r)
--
-- becomes the following :
--
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

-- what we are doing is basically
-- 1. Unpack r -> a out of Reader
-- 2. Compose f with the function we unpacked out of Reader
-- 3. Put the new function made from the composition back into the Reader
--

-- Implement the following "ask"
-- Write down what you know. What do you know about the 
-- type a ?
--
-- ask :: Reader a a 
-- ask = Reader

--
-- *Chap22> let f = (\c -> c  + 1)
-- *Chap22| 
-- *Chap22> let ask = Reader f
-- *Chap22| 
-- *Chap22> :t ask
-- ask :: Num a => Reader a a
-- *Chap22> runReader ask $ 5
-- 6
-- *Chap22> 
--

data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName,
  dogsAddress :: Address } deriving (Eq, Show)


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show) 
newtype Address = Address String deriving (Eq, Show)


pers = Person (HumanName "Big Bird") (DogName "Barkley") ( Address "Sesame Street")
ray = Person (HumanName "raymond tay") (DogName "lucky") ( Address "ghim moh road")

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--
getDogR = Dog <$> dogName <*> address
getDogR' = liftA2 Dog dogName address

--
-- Write liftA2 yourself. Think about it in terms of abstracting out the
-- different between getDogR and getDogR' if that helps.
--

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb 

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- example usage:
-- let f = (\x -> x + 1)
-- let a = asks f
-- runReader a $ 5
-- $> 6


-- 
-- Implement the Applicative for Reader
-- To write the Applicative instance for Reader, we will use a pragma
-- called InstanceSigs. It is an extension we need in order to 
-- assert a type for the typeclass methods. You ordinarily cannot assert 
-- type signatures in instances. The compiler already knows the type
-- of the functions, so it is not usually necessary to assert the types
-- in instances anyway. I did this for the sake of clarity to make
-- the Reader type explicitly in our signatures.
-- It needs to be placed at the first lines of the program, if you can recall.
--

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a) -- anon function returning the default value 'a'
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ (\r -> ((rab r) (ra r)))

-- Let's take some time to understand what just happened before?
-- Reader actually wraps a function, so we got that idea before.
-- Next, rab actually wraps a function "a -> b" and so does "ra".
-- and finally we apply the two functions "rab" and "ra" to the "r"
-- input to obtain "a -> b" and "a" respectively; ultimiately we apply
-- them together to obtain "b".
-- If we step back and look at the the problem ,we finally have a function
-- "r -> b" which is wrapped inside a Reader i.e. Reader (r -> b)
--

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ (\r -> ((runReader (aRb (ra r))) r))


