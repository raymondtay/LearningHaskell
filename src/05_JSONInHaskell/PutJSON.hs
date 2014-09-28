module PutJSON where

import Data.List (intercalate)
import SimpleJSON2

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber s)   = show s
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps) 
          renderPair (k,v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values xs = intercalate ", " (map renderJValue xs)

-- good haskell style involves separating pure code from code that performs I/O.
-- our renderJValue function has no interaction with the outside world, but we still
-- need to be able to print a JValue 

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

