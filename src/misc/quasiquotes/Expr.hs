{-# LANGUAGE DeriveDataTypeable #-}

--
-- From https://wiki.haskell.org/Quasiquotation#Syntax
--
module Expr(Expr(..), BinOp(..), eval, parseExpr) where

import Data.Generics
import Text.ParserCombinators.Parsec

data Expr = IntExpr Integer
          | AntiIntExpr String
          | BinopExpr BinOp Expr Expr
          | AntiExpr String
      deriving(Show, Typeable, Data)

data BinOp = AddOp | SubOp | MulOp | DivOp deriving (Show, Typeable, Data)

eval :: Expr -> Integer
eval (IntExpr n) = n
eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y)
  where
    opToFun AddOp = (+)
    opToFun SubOp = (-)
    opToFun MulOp = (*)
    opToFun DivOp = (/)


parseExpr :: Monad m => (String,Int,Int) -> String -> m Expr
parseExpr (file, line, col) s = 
	  case runParser p () "" s of
	    Left err -> fail $ show err
	    Right e -> return e
  where
    p = do pos <- getPosition
           setPosition $ 
              (flip setSourceName) file $ 
              (flip setSourceLine) line $ 
              (flip setSourceColumn) col $ 
              pos
           spaces
           e <- expr
           eof
           return e



















