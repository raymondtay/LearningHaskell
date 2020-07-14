import qualified Data.ByteString.Lazy.Char8 as L8 
import qualified Data.ByteString.Lazy as L
import Data.Word
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

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset } 

-- the following parses a single byte; the intention is aligned in the book
-- i.e. we are doing this over here..
parseByte :: Parse Word8
parseByte = 
    getState ==> \initState -> 
    case L.uncons (string initState) of 
        Nothing ->
            bail "end of input"
        Just (byte, remainder) ->
            putState newState ==> \_ -> identity byte
            where newState = initState { string = remainder, offset = newOffset }
                  newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right(s, s))

putState :: ParseState -> Parse()
putState s = Parse(\_ -> Right((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show  (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState = 
          case runParse firstParser initState of 
            Left errMessage -> Left errMessage
            Right (firstResult, newState) -> runParse (secondParser firstResult) newState

