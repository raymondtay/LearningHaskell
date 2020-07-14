module Rayson ( -- make sure the first letter of the module is a Capital letter
    JValue(..), -- the syntax name(..) means to export both the type and all of its constructors
    getString,
    getDouble,
    getObject,
    getInt,
    isNull,
    getArray) where

data JValue = JString String
    | JNumber Double
    | JBool Bool
    | JNull 
    | JObject [(String, JValue)]
    | JArray [JValue]
    deriving (Eq, Ord, Show)

getObject (JObject (kv:kvs)) = Just kv : getObject (JObject kvs)
getObject (JObject []) = [Just (show "nothing", JNull)]

getInt (JNumber x) = Just (truncate x)
getInt _ = Nothing

getDouble (JNumber x) = Just x
getDouble _ = Nothing

getString (JString s) = Just s
getString _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getArray (JArray arr) = Just arr
getArray _ = Nothing

isNull v = v == JNull

