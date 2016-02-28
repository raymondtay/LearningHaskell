module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Hello
import System.Random (randomRIO)

type WordList = [String] 

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w = 
          let l = length (w:: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO $ (,) 1 100
  return $ wl !! randomIndex

-- remember what it means to be Monad
-- by reading this signature (>>=) :: Monad m => m a -> (a -> m b) -> m b
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = do
  putStrLn "Enter your name: "
  name <- getLine
  sayHello name

