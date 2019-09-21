{-# LANGUAGE ViewPatterns #-}

module Numerals where

import Control.Monad.State
import Data.Foldable       (traverse_)
import Data.List           (groupBy)
import Data.Char           (isSpace, isDigit)


data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a ) (Expr a) deriving Show

-- Alternate encoding is to leverage `Show` typeclass.
-- instance Show a => Show (Expr a) where
--   show (Lit a) = show a
--   show (Add l r) = show l ++ " + " ++ show r
--   show (Mult l r) = show l ++ " * " ++ show r


type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type MyState = (Stack, Output)

isEmpty :: State MyState Bool
isEmpty = null <$> gets fst

notEmpty :: State MyState Bool
notEmpty = not <$> isEmpty

-- look at the top of the stack
--
top :: State MyState Token
top = gets (head . fst) -- let it crash on an empty stack

-- return an element from the top of the stack
--
pop :: State MyState Token
pop = do
  ~(t : s, es) <- get -- tilde / `~` character prefix is applied to pattern to claim its unfailable but i know it to be far from the truth
  put (s, es)
  pure t

-- like `pop` but discards the returning element
--
pop_ :: State MyState ()
pop_ = modify (\(s, es) -> (tail s, es))

-- store the token to the state
push :: Token -> State MyState ()
push t = modify (\(s, es) -> (t : s , es))

whileNotEmptyAnd :: (Token -> Bool) -> State MyState () -> State MyState ()
whileNotEmptyAnd pred m = go
  where
    go = do
      a <- notEmpty
      when a $ do
        b <- pred <$> top
        when b (m >> go)


output :: Token -> State MyState ()
output t = modify (builder t <$>)
  where
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder n es = Lit (read n) : es 

-- Precedence relationships can be defined
--
isOp "+" = True
isOp "*" = True
isOp _   = False

precedence "+" = 1
precedence "*" = 2
precedence _   = 0

op1 `precGTE` op2 = precedence op1 >= precedence op2

-- "shunting-yard" algorithm - Dijkstra resembles a lot like the 
convertToExpr :: String -> Expr Integer
convertToExpr str = head $ snd $ execState convert ([], [])
  where
    convert = traverse_ processToken (reverse $ tokenize str) >> transferRest
    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- quite literally a number

    transfer = pop >>= output
    transferWhile pred = whileNotEmptyAnd pred transfer
    transferRest = transferWhile (const True)

    tokenize = groupBy (\a b -> isDigit a && isDigit b) . filter (not . isSpace)

