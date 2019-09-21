module Polish2 where

import Control.Monad.State
import Data.Maybe
import Safe

type Stack = [Integer]

type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  stack <- get
  put (tailSafe stack)
  lift (headMay stack)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)

evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT evalRPN' []
  where 
    evalRPN' = traverse step (words expr) >> pop -- oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t = readSafe t >>= push
    processTops op = op <$> pop <*> pop >>= push

readSafe :: String -> EvalM Integer
readSafe s = lift (readMay s)

