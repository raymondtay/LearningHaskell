module PutJSON where

import Data.List (intercalate) 
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber s) = show s
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPairs ps) 
        renderPairs (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

-- Why should we separate the rendering code from the code that actually prints
-- a value ? This gives us flexibility. For instance, if we want to compress
-- the data before writing it out and intermix rendering with printing, it
-- would be much more difficult to adapt our code to that change in
-- circumstancs.
--
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)


