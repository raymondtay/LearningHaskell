{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SimpleJSON(
    JValue(..), -- all JValue type and data constructors
    getString, -- the named functions listed here are available for import, otherwise its hidden
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull
) where

data JValue = JString String
            | JNumber Double 
            | JBool Bool
            | JNull 
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

-- Extracts the string from the given JSON value
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray xs) = Just xs
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

