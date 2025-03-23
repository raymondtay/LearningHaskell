module Main where
import InteractWith (runInteractWith)
import FoldingWonders (asInt_fold)
import SimpleJSON
import PutJSON (putJValue)

main :: IO ()
main = do
  runInteractWith
  putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])

