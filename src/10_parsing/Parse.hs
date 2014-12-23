{- import qualified Data.ByteString.Lazy.Char8 as L8 -}
import qualified Data.ByteString.Lazy as L
import Data.Int 

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64 } deriving (Show)

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined 

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

{- 
Let's define a simple parser known as the identity parse. All it does is turn
whatever it is passed into the result of the parse. It looks like this
-}
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

{-
The first thing we must do is peel off the Parse wrapper so that we can get at the 
function inside. WE do so using the runParse function. We also need to construct a 
ParseState, and then run our parsing function on it. Finally, we'd like to separate the 
result of the parse from the final ParseState.
-}
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err -> Left err
        Right (result, _) -> Right result

