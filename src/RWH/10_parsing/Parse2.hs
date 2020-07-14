{-
  Using Functors for Parsing
  =================

  All this talk of functors has a purpose: they often let us write tidy, expressive code.

-}
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Word8
import Control.Applicative -- for the `<$>`
import Data.Char           -- for the `chr`
import Data.Int            -- for the `Int64`

data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64 } deriving (Show)

newtype Parse a = Parse { runParse :: ParseState -> Either String (a, ParseState) }

identity :: a -> Parse a
identity a = Parse(\s -> Right(a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of 
    Left  err         -> Left err
    Right (result, _) -> Right result

instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)
{-
  accessor functions can be used to alter state of data type (see below for illustration):
  ==========================================================================================

	*Main Control.Applicative Data.Char> let before = ParseState (L8.pack "foo") 9
	*Main Control.Applicative Data.Char|
	before :: ParseState
	(0.00 secs, 1035736 bytes)
	*Main Control.Applicative Data.Char> let after = modifyOffset before 4
	*Main Control.Applicative Data.Char|
	after :: ParseState
	(0.00 secs, 518976 bytes)
	*Main Control.Applicative Data.Char> before
	ParseState {string = "foo", offset = 9}
	it :: ParseState
	(0.00 secs, 1069496 bytes)
	*Main Control.Applicative Data.Char> after
	ParseState {string = "foo", offset = 4}
	it :: ParseState
	(0.00 secs, 1035520 bytes)
-}

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

-- A closure is simply the pairing of a funciton with its environment, 
-- the bound variables it can see. Closures are commonplace in Haskell.
-- In this case, the closure is not ujnwrapped till we apply `parse`
-- and our combinator here would stop on the first failure it sees. Neat!
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState = 
          case runParse firstParser initState of
            Left errMsg -> Left errMsg
            Right (firstResult, newState) -> runParse (secondParser firstResult) newState

-- Here's how you might possibly use 
-- ghci> let initState = ParseState (L8.pack "start") 0
-- ghci> runParse (bail "it fucking failed") initState
-- Left "byte offset 0: it fucking failed"

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse()
putState s = Parse(\_ -> Right((), s))

parseByte :: Parse Word8
parseByte = 
  getState ==> \initState ->
  case L.uncons (string initState) of
    Nothing -> bail "no more input"
    Just (byte, remainder) ->
      putState newState ==> \_ -> identity byte
      where newState = initState { string  = remainder, offset = newOffset }
            newOffset = offset initState + 1

{-

  To test whether Parse is truly a Functor, we need to provide two kinds of tests
  (a) identity
  (b) composability

  For (a), here's an expression that will take care of that
  > let input = L8.pack "hello there"
  > parse (id <$> parseByte) input (should return `Right 104`)
 
  For (b), here's an expression that will take care of that
  > parse ((chr . fromIntegral) <$> parseByte) input (should return `Right 'h'`) 

-}

w2c :: Word8 -> Char
w2c = chr . fromIntegral


