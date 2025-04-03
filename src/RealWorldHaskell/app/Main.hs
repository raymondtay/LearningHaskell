module Main where

import           ClassicIO
import           FoldingWonders     (asInt_fold)
import           InteractWith       (runInteractWith)
import           PutJSON            (putJValue)
import           SimpleJSON
import           System.Environment
import           TempFile           (doTempFile)
import           FamIOMonad

main :: IO ()
main = do
  args <- getArgs
  runInteractWith
  putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)]) -- chapter6
  doUpper -- chapter7
  doTempFile $ head args -- chapter7
  doStrAction

