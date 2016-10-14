{-#LANGUAGE InstanceSigs #-}

module Chap22_1 where

import Control.Applicative
import Data.Maybe
import Data.Char

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

{-
 - We aren't accustome to fmapping a function over another funciton
 - and you may be wondering what the functorial context here is.
 - By "functorial context", we mean the structure that the function is
 - being lifted over in order to apply to the value inside
 -
 - We say that the function gets lifted over the structure of the list
 - and applied to mapped over the values that are inside the list.
 -}
m' :: Integer -> Integer
m' = fmap durr $ fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr


{- 
 - See page 873-4 of the book for the context
 -}
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = fmap cap rev

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char],[Char])
tupled = (,) <$> composed <*> rev

--
-- The 'r' is the type we are 'reading' in and 'a' 
-- is the result type of our function.
--
newtype Reader r a = Reader { runReader :: r -> a }

--
-- 'ra' is actually a function and when applied to 'a'
-- gives 'a' which is in turn consumed by 'f' which returns
-- an 'b' and finally the expression \r -> f (ra r) becomes \r -> b
--
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  -- fmap f (Reader ra) = Reader $ \r -> f (ra r) is the same as the one below
  fmap f (Reader ra) = Reader $ f . ra

ask :: Reader a a
ask = Reader id

--
-- Write liftA2 yourself.
--
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb
--
-- Write the following function. Again, it is simpler than it looks.
--
asks :: (r -> a ) -> Reader r a
asks f = Reader f

-- Implement the Applicative for Reader
-- To write the Applicative instance for Reader, use a pragma
-- called InstanceSigs. It's an extension that's needed in order to assert
-- a type for the typeclass methods. You ordinarily cannot assert type
-- signatures in instances. The compiler already knows the type of the functions,
-- so its not usually necessary to assert the types in instances anyway.
--
-- when writing the `pure` function for Reader, remember that what you're trying to 
-- construct is a function that takes a value of type 'r', which you know 
-- nothing about, and return a value of type 'a'. Given that you're not really 
-- doing anything with 'r', there's really only one thing you can do.
--

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f = Reader $ \r -> (runReader $ f (ra r)) $ r

-- Speaking generally in terms of the algebras alone, you cannot get
-- a Monad instance from the Applicative. You can get an Applicative from the
-- Monad. However, our instances above arent' in terms of an abstract datatype
-- we know it's the type of functions. Because it's not hiding behind a Reader newtype
-- we can use `flip` and `apply` to make the Monad instance. We nede specific type
-- information to augment what the Appliance is capable of before we can get our
-- Monad instance.

-- 
-- At this point in time, its time to work thru the book's exercises
--
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- using `lookup`
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y 

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z 

zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- Have 'x1' make a tuple of 'xs' and 'ys'
-- Not comfortable with the '0' being there 
-- it should be deduced by default.
-- The thing with 'case'-expressions is that 
-- it conducts a conditional evaluation and depending
-- on the result, would trigger 
x1 :: Maybe (Integer, Integer)
x1 = 
  case (xs,ys) of
    (Just l, Just r) -> Just (l, r)
    (Just l, Nothing) -> Just (l, 0)
    (Nothing, Just r) -> Just (0, r)
    (_, _) -> Nothing     

x1' :: Maybe (Integer, Integer)
x1' = undefined

x2' :: Maybe (Integer, Integer)
x2' = undefined

x2 :: Maybe (Integer, Integer)
x2 = 
  case (ys,zs) of
    (Just l, Just r) -> Just (l, r)
    (Just l, Nothing) -> Just (l, 0)
    (Nothing, Just r) -> Just (0, r)
    (_, _) -> Nothing     

x3 :: Integer -> (Maybe Integer, Maybe Integer)
--x3 n = (,) <$> (z' n) <*> (z' n)
x3 n = (z' n, z' n) -- this works!
    
