-- A tiny parsing framework
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

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
-- the main idea is to encapsulate the idea of accumulating the successes while walking
-- the input and ignoring the problem.
--
many :: Parser a -> Parser [a]
many p = undefined


