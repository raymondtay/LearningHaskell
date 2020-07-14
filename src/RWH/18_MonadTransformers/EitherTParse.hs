module EitherTParse (
  Parseable,
  evalParseable
  ) where

import MaybeTParse


import Control.Monad.Except
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

-- Copied [[ParseState]] verbatim from MaybeTParse.hs
--
data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64
} deriving (Show)

-- Corresponds to the RWH exercise on page 441 for the chapter on Monad
-- Transformers. This is my rendition. I've used type parameters 'a' and 'e' to
-- represent the Right and Left respectively.
--
newtype Parseable e a = ExceptTParse { runParseable :: ExceptT e (State ParseState) a }

evalParseable :: Parseable e a -> L.ByteString -> Either e a
evalParseable m s = evalState (runExceptT (runParseable m)) (ParseState s 0)

{-

  Exercise
  ---------
    --
    -- Our `Parse` monad is not a perfect replacement for its earlier
    -- counterpart. Because we are using `Maybe` instead of `Either` to
    -- represent a result, we cannot report any useful information if a parse
    -- fails.
    --
    -- Create an `EitherT sometype` monad transformer, and use it to implement
    -- a more capable `Parse` monad that can report an error message if parsing
    -- fails.
    --
    -- If you like to explore the Haskell libraries for fun, you may have run
    -- across an existing `Monad` instance for the `Either` type in the
    -- `Control.Monad.Error` module. We suggest that you do not use that as a
    -- guide. It's design is too restrictive: it turns Either String into
    -- Monad, when you could use a type parameter instead of a String.
-}
