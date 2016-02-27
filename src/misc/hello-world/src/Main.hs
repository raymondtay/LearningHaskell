module Main where

import Hello

main :: IO ()
main = do
  putStrLn "Enter your name: "
  name <- getLine
  sayHello name

