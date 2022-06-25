{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module WhySomeException where

import           Control.Exception (ArithException (..), AsyncException (..))
import           Criterion.Main
import           Data.Typeable

data MyException = forall e . (Show e, Typeable e) => MyException e
instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

data SomeError =
  Arith ArithException | Async AsyncException | SomethingElse deriving (Show)

discriminateError:: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing ->
      case cast e of
        (Just async) -> Async async
        Nothing      -> SomethingElse

runDisc n = either discriminateError (const SomethingElse) (multiError n)

-- This is the essence of why we need existential quantificaiton for
-- exceptions - so that we can throw various exceptions types without
-- being forced to centralize and unify them under a sum type. Don't
-- abuse this facility.

main :: IO ()
main = defaultMain
  [
    bench "multiple errors 1" $ whnf runDisc 0,
    bench "multiple errors 2" $ whnf runDisc 1
  ]
