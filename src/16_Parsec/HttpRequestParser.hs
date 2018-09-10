module HttpRequestParser (
  HttpRequest(..),
  Method(..),
  p_request,
  p_query
  ) where

import Numeric (readHex)
import Control.Applicative (liftA2)
import Control.Monad (liftM4)
import System.IO (Handle)
import Text.ParserCombinators.Parsec

-- An http request consists of a method , an identifier, a series of headers
-- and an optional body. For simplicity, we will focus on just two of the six
-- method types specified by the HTTP 1.1 standard. A post method has a body; a
-- GET has none:
--
data Method = Get | Post deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
reqMethod :: Method,
reqUrl :: String,
reqHeaders :: [(String,String)],
reqBody :: Maybe String 
                               } deriving (Eq, Show)

-- We are going to write this in an applicative style, which means the parser
-- can be brief and readable; and by readable we mean that you should find it
-- easier to read once you are used to the applicative parsing notation.

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
    where q name ctor body = liftM4 HttpRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
          url = optional (char '/') *>
                manyTill notEOL (try $ string " HTTP /1." <* oneOf "01")
                <* crlf

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

p_headers :: CharParser st [(String,String)]
p_headers = header `manyTill` crlf
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        contents = liftA2 (++) (many1 notEOL <* crlf) (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
        fieldName = (:) <$> letter <*> many fieldChar
        fieldChar = letter <|> digit <|> oneOf "-_" 

-- Backtracking
--
-- The "try" combinator has to hold onto input in case it needs to restore it
-- so that an alternative parser can be used. This practice is referred to as
-- backtracking. Because try must save input, it is expensive to use.
-- Sprinkling a parser with unnecessary uses of try is a very effective way to
-- slow it down, sometimes to the point of unacceptable performance.
--
-- The standard way to avoid the need for backracking is to tidy up a parser so
-- that we can decide whether it will succeed or fail using only a single token
-- of input. In this the two parsers consume the same initial tokens, so we
-- turn them into a single parser:
--
-- import Text.ParserCombinators.Parsec
-- let parser = (++) <$> string "HT" <*> (string "TP" <|> string "ML")
-- in parse parser "(unknown)" "HTTP"
--

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name  <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars <|> (char '+' >> return ' ') <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

