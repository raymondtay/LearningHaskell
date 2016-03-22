
module Chap18 where

import Control.Monad (join, liftM, liftM2)

import Control.Applicative

{--
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


