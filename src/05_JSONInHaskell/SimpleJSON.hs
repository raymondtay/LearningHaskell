-- For each JSON type, we supply a distinct value constructor. 
-- Some of these constructors have parametrs: if we want to construct a JSON
-- string, we must provide a String value as a argument to the JString constructor
data JValue = JString String
        | JNumber Double
        | JBool Bool
        | JNull
        | JObject [(String, JValue)]
        | JArray [JValue]
        deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

-- let jsonString = [JString "hi", JNumber 1.22] in getString (head jsonString)

getInt (JNumber n) = Just (truncate n )
getInt _           = Nothing

-- let jsonString = [JString "hi", JNumber 1.22] in getInt (head (tail jsonString))

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v = v == JNull



