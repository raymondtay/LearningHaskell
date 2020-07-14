-- A tiny parsing framework
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- probably a bad idea but for now, seems harmless.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

import Control.Applicative hiding (many)
import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

data ParseError = NumericOverflow | EndOfInput | Chatty String deriving (Eq, Ord, Show)
instance Error ParseError where
  noMsg  = Chatty "oh noes!"
  strMsg = Chatty

-- For our parser's state, we will create a very small monad transformer stack.
-- A stack monad carries around the ByteString to parse, and ExceptT is stacked
-- on top to provide error handling
--

-- Page 464 says:
-- As usual, we wrapped our monad stack in a newtype. Costs us nothing in
-- performance but adds type safety. We deliberately avoided deriving an
-- instance of MonadState B.ByteString. This means that users of the Parser
-- monad will not be able to use get or put to query or modify the parser's
-- state. As a result, we force ourselves to do some manual lifting to get at
-- the State monad in our stack.
--
newtype Parser a = P {
  runP :: ErrorT ParseError (State B.ByteString) a
} deriving (Functor, Applicative, Monad, MonadError ParseError)

liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get -- get the state and lift it into "P"
  case B.uncons s of
      Nothing -> throwError EndOfInput
      Just (c, s')
        | p c       -> liftP (put s') >> return c
        | otherwise -> throwError (Chatty "satisfy failed")

-- The catchError function is useful for tasks beyond simple error handling.
-- For instance, we can easily defang an exception, turning it into a more
-- friendly form:

optional :: Parser a -> Parser (Maybe a)
optional p = liftM Just p `catchError` \_ -> return Nothing -- when in error, return Nothing.

-- To execute this, we plug all these functions together 
--
runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runErrorT (runP p)) bs of
                     (Left err, _) -> Left err
                     (Right r, bs) -> Right(r, bs)

-- `many` should apply a parser until it fails
-- Got the following idea when scanning GHC's codebase in the
-- [[ParserCombinators]]. I cannot use [[runParser]] as defined above to
-- compose this as that function needs the data strings to be present and the
-- idea is not to do that just yet - i need to figure out a way to build an
-- abstraction that represents the intention to push the parsing.
--

many :: Parser a -> Parser [a]
many p = return [] +++ many1 p

-- This is a key abstraction that represents the idea of "merging" the first
-- parser we see i.e. "p" here, whilst intending to proceed further with the
-- remaining of the parsing actions i.e. "(many p)" here.
--
many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

-- Now we need to define the idea of running 2 parsers and picking either one
-- to push forth...the key understanding here is to leverage `Alternative`
-- typeclass and to be specific its to use `<|>`.
-- here 'a' is 
--
-- Reference: https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
--
(+++) :: Parser a -> Parser a -> Parser a
(+++) (P a) (P b) = P (a <|> b)


