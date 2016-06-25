
module AltParsing where

import Control.Applicative
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah blah"
b = "11111"
c = "123123blah lbah 12132"

--
-- we can read <|> as being an "or" or disjunction, of our two parsers;
-- `many` is "Zero or more" and `some` is "one or more".
--
parseNumbers :: Parser NumberOrString
parseNumbers  = (Left <$> integer) <|> (Right <$> some letter)

{-
 - From the GHCI console where we import Trifecta, i ran some commands
 - just to get a feel of the library:
 - *AltParsing Text.Trifecta> parseString (some integer) mempty "111"
 - Success [111]
 - *AltParsing Text.Trifecta> parseString (some integer) mempty "aaa"
 - Failure (interactive):1:1: error: expected: integer
 - aaa<EOF>
 - ^
 -}

