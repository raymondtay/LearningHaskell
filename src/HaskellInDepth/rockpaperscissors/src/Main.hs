module Main where

import RulesNModels
import System.Random
import Control.Monad.State (evalState)

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 20) g
  print r


