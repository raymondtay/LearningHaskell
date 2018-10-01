module FormParse where

import Numeric -- for 'readHex'
import Control.Monad
import Text.ParserCombinators.Parsec

--instance MonadPlus (GenParser tok st) where
  --mzero = fail "mzero"
  --mplus = (<|>)

-- parsing a url-encoded query string
-- simple example of parsing a http header string like
-- "application/x-www-form-urlencoded".
p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

-- The HTTP specification is unclear about whether a key must have an
-- associated value, and we would like to be able to distinguish between "no
-- value" and "empty value"
--

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

-- lift the previous function.
p_pair' = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
      <|> (char '+' >> return ' ')
      <|> p_hex

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _) : _) = readHex [a,b]
  return . toEnum $ d

-- make sure to check out 'parseTest' in Parsec on how to check for stuff.
-- e.g. parseTest p_query "foo=bar&a%21=b+c" but i am curious as to 
-- why the author(s) of this library decided to ship this test function in the
-- package.

