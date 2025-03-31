module Main where
import           ClassicIO
import           FoldingWonders (asInt_fold)
import           InteractWith   (runInteractWith)
import           PutJSON        (putJValue)
import           SimpleJSON

main :: IO ()
main = do
  runInteractWith
  putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
  doUpper

