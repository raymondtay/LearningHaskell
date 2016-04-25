
module Chap18 where

import Control.Monad
import Control.Applicative
import Test.QuickCheck
-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes

{-
Concentrating on the defn of `bind` or `>>=`:
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
                       m    a -> (a ->  m     b) -> m b
Prelude Control.Monad> Just 1 >>= \x -> Just (x+1)
Just 2

Again, let's map types: m      a        -> (a  -> m     b)
Prelude Control.Monad> Just (\x -> x+1) >>= \f -> Just (f 4) -> m b 
Just 5

 - To understand how (>>=) works in monads
 - its good to start with a simple example 
 - and work our way up
 -}

result :: [[Integer]]
result = let add1 x = [x, 1] in add1 <$> [1..4]

final_result :: [Integer]
final_result = join result
{-
 - What can we understand from the previous example?
 - If we break it down, we find that "bind"
 - is nothing more than applying "concat" or "join" 
 - to the result of mapping (i.e. "fmap") a function 
 - (in this case, "add1") to a monad (in this case is a list of integers)
 - and we can verify this by doing the following:
 -
 - [1..4] >>= add1 and the result is the same as "final_result"
 -}

{-
 - "bind" in Monad is defined as (>>=) 
 - and our job is to re-define it using fmap
 - and join
 -}
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mm = join $ f <$> mm

{-
 - Trying out liftA, liftA2 etc
 - Trying out liftM, liftM2 etc
 - just to get a "feel" on how to actually lift functions
 -}

-- liftA
add1 x = [x,1]
f = liftA add1
result_a = f [1..4]
flattened_result_a = join result_a

-- liftA2
add2 x y = [x+1, y+2]
g = liftA2 add2
result_b = g [1..3] [1..3]
flattened_result_b = join result_b

-- liftM
h = liftM id
result_c = h [1] -- simply [1]

-- liftM2
i = liftM2 (,)
result_d = i (Just 1) (Just 2) -- Just (1,2)

-- (>>) basically means evaluate both LHS and RHS but return RHS only
result_e :: [Integer]
result_e = ((+1) <$> [2]) *> [4]

-- Some stuff posted on the book, repeatedly below
-- which basically demonstrates the equivalence of "do", ">>" & "*>".
sequencing :: IO ()
sequencing = do
  putStrLn "a"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "a" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = putStrLn "a" *> putStrLn "another thing"

--
-- The following 2 definitions binding and binding'
-- are equivalent.
--
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

--
-- The following 2 definitions bindingAndSequencing and bindingAndSequencing'
-- are equivalent.
--
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name please:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = 
  putStrLn "name please: " >>
  getLine >>= \name -> putStrLn ("y helo thar: " ++ name)

--
-- The following 2 definitions twoBinds and twoBinds'
-- are equivalent.
--
twoBinds :: IO ()
twoBinds = do
  putStrLn "name please: " 
  name <- getLine
  putStrLn "age please: " 
  age <- getLine
  putStrLn("y helo thar: " ++ name ++ " who is : " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' = do
  putStrLn "name please: " >>
    getLine >>= \name ->
      putStrLn "age please: " >>
        getLine >>= \age ->
          putStrLn("y helo thar: " ++ name ++ " who is : " ++ age ++ " years old")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

data Cow = Cow {
name :: String,
age :: Int,
weight :: Int } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n
  | n >= 0    = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = 
  case noEmpty name' of
    Nothing -> Nothing
    Just x ->
      case nonNegative age' of
        Nothing -> Nothing
        Just a  ->
          case nonNegative weight' of
            Nothing -> Nothing
            Just w  -> weightCheck (Cow x a w)

--
-- 'do' isn't just for IO related actions 
-- as commonly depicted in haskell books
--
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' =
  do
    n <- noEmpty name'
    a <- nonNegative age'
    w <- nonNegative weight'
    weightCheck (Cow n a  w)

-- 
-- we can write the above definition using (>>=)
--
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \n -> 
      nonNegative age' >>=
        \a -> 
          nonNegative weight' >>=
            \w -> weightCheck (Cow n a w)

-- 
-- Composing monads
--
monad_comp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
monad_comp f g a = g a >>= f -- equivalent to (g a) >>= f i.e. eval (g a) and the resulting monad is passed to f

-- 
-- In ordinary function composition and >>=
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--
-- To get Kleisli composition off the ground, we have to flip 
-- some arguments around to make the types work:
--
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) 
--

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

--
-- basically, its lifting the read function to
-- a monad 
--
readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?" 

--
-- The following code below demonstrates hw
-- Monads are constructed and validated 
-- as part of the chapter's exercises
--
data Nope a = NopeDotJpg
instance Functor Nope where
  fmap f _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  l >>= f = NopeDotJpg

-- instance Arbitrary (Nope a) where

data ButEither b a = 
  Left a
  | Right b

instance Functor (ButEither b) where
  fmap f (Chap18.Left a) = Chap18.Left (f a)

{-

import Control.Monad (ap)

(<*>) == ap

-- keeping in mind
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ap    :: Monad m       => m (a -> b) -> m a -> m b
ap m m' = do
  x  <- m -- "extracting" the function
  x' <- m' -- "extracting" the monad
  return (x x') -- function application btw
Control.Monad> Just (\x -> x + 1) <*> Just 4
Just 5
Control.Monad> ap (Just (\x -> x + 1)) $ Just 4
Just 5
```
behavior if derived from the Monad instance's bind operation.
-}

data Sum a b = 
  First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure x = Second x
  (Second f) <*> (Second g) = f <$> Second g
  _ <*> (First a)  = First a

instance Monad (Sum a) where
  return = pure
  (Second a) >>= f = f a

--
-- Wriet a Monad instance for Identity
--
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = (f a)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

instance Applicative List where
  pure x = Cons x Nil
  (Cons f xs) <*> (Cons y ys) = Cons (f y) $ (Cons f xs) <*> ys

--
-- Monad Composition
--
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = (g a) >>= f
-- Why did it work?
-- when we apply g to a i.e. (g a) it returns a monad which is m b
-- and u need to realize that we are basically working on a monad 
-- and f which is `m b` and `(b -> m c) ` respectively so that 
-- we can merge them together.
-- Turns out i don't have to learn how to do that,because its already
-- there in the Control.Monad package which is known as (>=>)

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = f <$> ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b) -- `return` is necessary s.t. it can comply with the type signature in Monad

a :: Monad m => m a -> m (a -> b) -> m b
a ma fma = do
  f <- fma
  a' <- ma
  return (f a')-- `return` is necessary s.t. it can comply with the type signature in Monad

