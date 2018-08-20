module Transformers where

import Control.Monad.Identity
import Control.Monad.Error -- its already deprecated in GHC 8.4.3
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe

import qualified Data.Map as Map

-- A simple language where `Name`, `Exp` and `Value` are part of the tokens of
-- the language. The interpreter is defined in `eval0`.
--
type Name = String -- variable names

data Exp = Lit Integer -- expressions
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value = IntVal Integer | FunVal Env Name Exp deriving (Show) -- values

type Env = Map.Map Name Value -- mapping from names to values

-- first attempt at the interpreter of this algebra
--

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)

eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of FunVal env' n body -> eval0 (Map.insert n val2 env') body
{-
Run the following and it still works !
*> exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
*> eval0 Map.empty exampleExp
IntVal 18

-}



-- The goal of using monad transformers is to have control over aspects of
-- computations,such as state, errors environments etc. It is a bit tedious to
-- reformulate an already written program in monadic style, but once that is
-- done, it is relatively easy to add, remove or change the monads involved.
--
--
-- Converting to Monadic style ...
--
-- In order to use monad transformers, it is necessary to express functions in
-- monadic style. that means that the programmer needs to impose sequencing on
-- all monadic operations using `do` notation, and to use the `return` function
-- in order to specify the result of a function.
--
--

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a 
runEval1 ev = runIdentity ev

-- based on the Eval1 monad, we now rewrite the eval0 function as eval1:

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)

eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                               FunVal env' n body -> eval1 (Map.insert n val2 env') body

{-
Run the following and it still works !
*> exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
*> runEval1 (eval1 Map.empty exampleExp)
IntVal 18

-}
-- Adding error handling
-- Using monad transformers, we simply go to our local monad transformer
-- library and take the ErrorT monad transformer, using it to extend the basic
-- Eval1 monad to Eval2.
--
type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

-- We can now simply change the type of our eval1 function, giving the
-- following version, called eval2a.

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust (Map.lookup n env)
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                FunVal env' n body -> eval2a (Map.insert n val2 env') body

-- This version can be run using the runEval2 function defined above. When we
-- apply this function to our example expression, the result only varies in
-- that it is wrapped in a Right constructor:
--

-- But unfortunately, when given an invalid expression the error reporting of
-- the ErrorT transofmr is not used. We have to modify our definition in
-- order to give useful error messages:
--

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = return $ fromJust (Map.lookup n env)
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                                 (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                 _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                                FunVal env' n body -> eval2b (Map.insert n val2 env') body

eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = return $ fromJust (Map.lookup n env)
eval2c env (Plus e1 e2) = do IntVal i1 <- eval2c env e1
                             IntVal i2 <- eval2c env e2
                             return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do FunVal env' n body <- eval2c env e1
                            val2 <- eval2c env e2
                            eval2c (Map.insert n val2 env') body


-- The drawback of this funciton is that the error messages only talks about
-- poattern matching failure, with no specific information about why the
-- pattern match fails. Thus, in order to get good error messages, it is better
-- to provide our own calls to throwError. This is what we will do for the
-- final version of the error handling evaluation.
--
--
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                                (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                               FunVal env' n body -> eval2 (Map.insert n val2 env') body
                               _ -> throwError "type error in application"

-- Note: The Control.Monad.ERror module provides another function for catching
-- errors raised using throwError called catchError : m a -> (e -> m a) -> m a
-- for arbitrary error monads. It can be used for either handling errors
-- locally or passing them on to the surrounding calling context. 
--
--
--
-- Hiding the environment 
-- one way to make the definition of the evaluation fu nction even more
-- pleasing is to hide the environemnt from all function definitions and calls.
-- Since there is only one place where the environment is extended and two
-- places where it is actually usedf (for variables and λ expressions), we can
-- reduce the amount of code by hiding it in all other places.
--
--
-- This will be done by adding a ReaderT monad transformaer in order to
-- implement a reader monad. A reader monad passes a value into a computation
-- and all its sub-computations. This contraste to state monads, an
-- encapsulated computation cannot change the value used by surrounding
-- computation.
--
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                               FunVal env' n body -> eval0 (Map.insert n val2 env') body


main = do
  putStrLn ""
