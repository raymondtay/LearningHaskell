module UseRayson where

import Data.List (intercalate)
import Rayson

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber s) = show s
renderJValue (JBool True) = show True
renderJValue (JBool False) = show False
renderJValue JNull = show "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)

-- Why shoudl we separate the rendering code from the code that actually 
-- prints a value? As the book argues, which i am inclined to agree, 
-- is that it gives us, developers, flexibility.
-- The idea of separating pure from impure code is powerful, and it is
-- pervasive in Haskell code. 
putJValue v = putStrLn (renderJValue v)

data Doc = undefined

smallHex :: Int -> Doc
smallHex x = text "\\u"
    <> text (replicate (4 - length h) '0')
    <> text h 
    where h = showHex x ""
