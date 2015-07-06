{-
  Using Functors for Parsing
  =================

  All this talk of functors has a purpose: they often let us write tidy, expressive code.

-}
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
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

{-
instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char 
parseChar = w2c <$> parseByte

-}
