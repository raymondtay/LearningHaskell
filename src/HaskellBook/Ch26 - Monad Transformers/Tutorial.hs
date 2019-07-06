{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, InstanceSigs #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import Data.Char

import Control.Monad.Trans.Class -- class of transformers

-- Problem statement:
--
-- I want a feature that allows me to ask the user
-- for input, and if its not a number i want to notify the user of the error
-- and if its indeed a number then i want to add this number to my data
-- structure and ask the user again
--

data UserInput a = Input { numbers :: [a] } deriving (Eq, Show, Functor)

instance Applicative UserInput where
  pure :: a -> UserInput a
  pure a = Input [a]
  (<*>) :: UserInput (a -> b) -> UserInput a -> UserInput b
  (Input xfs) <*> (Input xs) = Input $ xfs <*> xs

instance Monad UserInput where
  return :: a -> UserInput a
  return = pure
  (>>=) :: UserInput a -> (a -> UserInput b) -> UserInput b
  (Input xs) >>= f = Input $ do
    x <- xs
    numbers $ f x

-- now that we have prepared all the instances, i want to now add error
-- handling to my new monad `UserInput`.
--

validateInputIsNumber :: String -> IO Bool
validateInputIsNumber = return . all isNumber

data InputError = NotANumber | SomePartsNotANumber deriving (Show, Eq)

type InputMonad = ExceptT InputError IO

validateInputIsNumberT :: String -> InputMonad Bool
validateInputIsNumberT x = case (all isNumber x) of
                               False -> throwError NotANumber
                               True -> return True

addIt :: Int -> State (UserInput Int) Int
addIt x = do
  ui <- get
  put . Input $ (numbers ui) ++ [x+1]
  ui' <- get
  return (last . numbers $ ui')

main2 :: IO (Int, UserInput Int)
main2 = do
  input <- getLine
  let init = Input [0]
  result <- validateInputIsNumber input
  case result of
      True -> return $ runState (addIt (read input :: Int)) init
      False -> return (0, init)

main :: IO (Int, UserInput Int)
main = do
  putStrLn "Enter a number"
  input <- liftIO getLine
  let init = Input [0]
  result <- runExceptT $ validateInputIsNumberT input
  return (0, init)
  case result of
      (Right True) -> return $ runState (addIt (read input :: Int)) init
      otherwise -> do {putStrLn "Not gonna work";return (0, init)}
  

type Parser = StateT String []

runParser :: Parser a -> String -> [a]
runParser p s = [x | (x, "") <- runStateT p s]

item :: Parser Char
item = do
  c : cs <- get
  put cs
  return c


