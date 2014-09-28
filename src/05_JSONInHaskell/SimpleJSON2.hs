
-- for each JSON data type, we supply a distinct value constructor. Somer of these construcotrs
-- have parameters; if we want to construct a JSONM string, we must provide a Stirng value
-- as an argument to the JString constructor.
-- the (..) indicates that we are exporting both the type and all of its constructors

module SimpleJSON2 (JValue(..)) where 
--    ( JValue(..), getString, getInt, getDouble, getBoolean, getObject, getArray, isNull) where

data JValue = JString String
                | JNumber Double
                | JBool Bool
                | JNull 
                | JObject [(String, JValue)]
                | JArray [JValue]
                deriving (Eq, Ord, Show)

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])


