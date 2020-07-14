module Transformers where

{-
  |
  | Source code for "Monad Transformers Step by Step" - Oct 2006 Martin Grabmuller
  | Updates:
  |   - Source code updated for GHC 8.4.3
  |
-}

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Except
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

  Run the following and it works !
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

type Eval1 a = Identity a -- First thing to do
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
-- library and take the ExceptT monad transformer, using it to extend the basic
-- Eval1 monad to Eval2.
--
type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

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
-- the ExceptT transofmr is not used. We have to modify our definition in
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

-- unicode for alpha is '03b1' and lambda is '03bb' i.e. α λ
type Eval3 α = ReaderT Env (ExceptT String Identity) α

runEval3 :: Env -> Eval3 α -> Either String α
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return (IntVal i)
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                       Nothing -> throwError ("unbound variable: " ++ show n)
                       Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return (IntVal $ i2 + i1)
                            _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return (FunVal env n e)
                     
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                           _ -> throwError "type error in application"


{-
  Adding State
  Another important application of monads is to provide mutable state to otherwise purely functional code.
  This can be done using a State Monad, which provides operations for specifying an initial state, querying 
  the current state and changing it.

-}

type Eval4 α = ReaderT Env (ExceptT String (StateT Integer Identity)) α

-- The return type of the function `runEval4` changes, because the final state
-- is returned together with the evaluation result (error or value).
-- Additionally, we give the initial state as an additional parameter so that
-- we gain some flexibility (this can be used, for example, to start a
-- computation in the final state of another one).
--
--
-- Note: When you study the declaration of type versus the types of extracting
-- the monad transformer, you would realize its the reversed of one another.
--
runEval4 :: Env -> Integer -> Eval4 α -> (Either String α, Integer)
runEval4 env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

-- As before, to run this: runStateT tick 4 should return you ((), 5)
-- By adding a call to the "tick" function in each case, we can count the
-- number of applications.

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return (IntVal i)
eval4 (Var n) = do env <- ask
                   tick
                   case Map.lookup n env of
                       Nothing -> throwError ("unbound variable: " ++ show n)
                       Just val -> return val
eval4 (Plus e1 e2) = do e1' <- eval4 e1
                        e2' <- eval4 e2
                        tick
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return (IntVal $ i2 + i1)
                            _ -> throwError "type error in addition"
eval4 (Abs n e) = do env <- ask
                     tick
                     return (FunVal env n e)
                     
eval4 (App e1 e2) = do val1 <- eval4 e1
                       val2 <- eval4 e2
                       tick
                       case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                           _ -> throwError "type error in application"

tick' :: (Num s, MonadState s m) => m ()
tick' = do st <- get
           modify(\s -> s+2)

tick'' :: (Num s, MonadState s m) => m ()
tick'' = do tick -- ← reuses/combines "tick" here, pretty nifty isn't it?
            modify(\s -> s+2)
-- As what happened to "tick'": runStateT tick' 4 should return you ((), 6)
-- As what happened to "tick''": runStateT tick' 4 should return you ((), 7)
--


-- Adding Logging
--
-- The last monad transformer in the toolbox is WriterT. It is ain some sense
-- dual to ReaderT because the functions it provides let you add values to the
-- result of the computation istead of using some values passed in.
--
type Eval5 α = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) α

-- Similar to StateT, WriterT interacts with ExceptT because it produces
-- output. So depending on the order of ExceptT and WriterT, the result will
-- include the values written out or not when an error occurs. The values to be
-- written out will be lists of strings. When you read the documentation for
-- the WriterT monad transformer, you will notice that the type of the output
-- values is restricted to be a member of the type class Monoid. This is
-- necessary because the methods of this class are used internally to construct
-- the initial value and to combine several values written out.
--

runEval5 :: Env -> Integer -> Eval5 α -> ((Either String α, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT(runExceptT (runReaderT ev env))) st)

-- Here's how we can leverage the WriterT in the "eval" function.
--

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return (IntVal i)
eval5 (Var n) = do env <- ask
                   tick
                   tell [n]
                   case Map.lookup n env of
                       Nothing -> throwError ("unbound variable: " ++ show n)
                       Just val -> return val
eval5 (Plus e1 e2) = do e1' <- eval5 e1
                        e2' <- eval5 e2
                        tick
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return (IntVal $ i2 + i1)
                            _ -> throwError "type error in addition"
eval5 (Abs n e) = do env <- ask
                     tick
                     return (FunVal env n e)
                     
eval5 (App e1 e2) = do val1 <- eval5 e1
                       val2 <- eval5 e2
                       tick
                       case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                           _ -> throwError "type error in application"

-- What about I/O ?
--
-- How do we integrate I/o into the monadic definitions we have developed so
-- far? It's not possible to define an I/O monad transformer, because in the
-- execution of the I/O operations in Haskell cannot be arbitrarily nested into
-- other nested functions or monads, they are only allowed in the monad IO.
-- Fortunately, the monad transformer library provides us with the
-- infrastructure to easily integrate I/O operations into our framework: simply
-- substitute IO where we have used Identity. 
--
-- This is possible because Identity is the base monad, the function
-- runIdentity for evaluating actions in this monad is always applied last.
--

type Eval6 α = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) α

-- the return type of runEval6 is wrapped in an IO constructor, which means
-- that the running an Eval6 computation does not directly yield a result, but
-- an i/o computation which must be run in order to get at the result. 
--

runEval6 :: Env -> Integer -> Eval6 α -> IO ((Either String α, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO (print i) -- escape hatch !
                   return (IntVal i)
eval6 (Var n) = do env <- ask
                   tick
                   tell [n]
                   case Map.lookup n env of
                       Nothing -> throwError ("unbound variable: " ++ show n)
                       Just val -> return val
eval6 (Plus e1 e2) = do e1' <- eval6 e1
                        e2' <- eval6 e2
                        tick
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return (IntVal $ i2 + i1)
                            _ -> throwError "type error in addition"
eval6 (Abs n e) = do env <- ask
                     tick
                     return (FunVal env n e)
                     
eval6 (App e1 e2) = do val1 <- eval6 e1
                       val2 <- eval6 e2
                       tick
                       case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                           _ -> throwError "type error in application"

