{-# LANGUAGE InstanceSigs #-}

module Chapter_22 where

import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

--
-- Aren't readers just function composition ???
-- What's the hoo-haa about then?
--
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)
  --fmap f (Reader g) = Reader $ \x -> f (g x)

-- On page 882 of the book, authors requested the reader to try out the
-- exercise and i decided to do so and now after having understand most of the
-- symbolics involved, its easier to complete the exercise. So, let's see how
-- it was actually constructed?
--
-- First, i must understand that Reader r (a -> b) really means r -> (a -> b)
-- which is a function that returns another function. Once i understood that,
-- then it is almost trivial to construct the definition; how so? In the defn,
-- its clear that `f` represents `r -> (a -> b)` and so `f x` would,
-- undoubtedly, return `a -> b` and `g x` would return `a` which is fed to the
-- function `a -> b`. Its clear now right?
--
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\_ -> a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader f) (Reader g) = Reader $ (\x -> ((f x)(g x)))

--
-- Reader a a is really a -> a where we replace 'r' with 'a'; then it should be
-- clear.
--
ask :: Reader a a
ask = Reader id

-- Let's get familiar with how YOU might use Readers
--
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show) 
newtype Address = Address String deriving (Eq, Show)

-- Strings are just strings but since ANYthing and everything can be strings,
-- we want types to be associated with Strings so that they make sense
--
data Person =
  Person {
  humanName :: HumanName,
  dogName :: DogName,
  address:: Address
         } deriving (Eq, Show)


data Dog = 
  Dog {
  dogsName :: DogName,
  dogsAddress :: Address 
      } deriving (Eq, Show)


bigBird = Person (HumanName "bird bird") (DogName "barkley") (Address "sesame street")

raymond = Person (HumanName "Raymond Tay") (DogName "lucky") (Address "singapore")

-- without the Reader style, we would write the following
--
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)


-- with Reader style, we would write the following
--
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- another way to write the same thing ↑ 
-- but you need to import Control.Applicative 
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- The implementation isn't hard once i understood how getDogR and getDog works
-- in relation to one another.
--
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f l r = f <$> l <*> r

myLiftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
myLiftA3 f a b c = f <$> a <*> b <*> c

