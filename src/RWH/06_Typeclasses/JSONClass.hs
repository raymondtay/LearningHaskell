
import Control.Arrow (second)

class JSON a where
    toJValue :: a -> JSON
    fromJValue :: [JValue] -> JSON

newtype JAry a = JAry {
    fromJAry :: [a]
} deriving (Ord, Eq, Show)

newtype JObj a = JObj {
    fromObj :: [(String, a)]
} deriving (Eq, Ord, Show)

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromObj
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k,v) = whenRight ((,), k) (fromJValue v)
    fromJValue _ = Left "not a json object"


data JValue = JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue)
    | JArray (JAry JValue)
    deriving (Eq, Ord, Show)

type JSONError = String

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

-- whenever it encounters a Left value, it returns immediately instead of accumulate
-- a list of Right values.
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of 
        Left err -> Left err
        Right y -> Right (y:ys)
mapEithers _ _ = Right []

-- this has no performance cost. we're just telling the compiler to hide
-- the fact that we're using a list. to turn this into a JValue, we apply
-- another type constructor.
jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue (JArray (JAry a)) = 
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

