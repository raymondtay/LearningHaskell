module MyModule where

-- |
-- | Here's how to develop the transaction MVars which is what TMVar is really
-- | about.
-- |
import Control.Concurrent.STM hiding (newEmptyTMVar, takeTMVar, putTMVar, TMVar)
import Control.Monad.IO.Class

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a) 
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)

-- takeMVar blocks if the desired variable is empty and returns the content
-- once the variable is set.
takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
      Nothing -> retry
      Just a -> do
        writeTVar t Nothing
        return a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
      Nothing -> do
        writeTVar t (Just a)
        return ()
      Just _ -> retry


-- if you comment either (1) or (2) and run/re-run this, you will noticed quite
-- immediately that Ghci will terminate the transaction with a complaint:
--
-- " thread blocked indefinitely in an STM transaction " after about 5 seconds;
-- 
-- and that's proof that the documentation is correct because we know its going
-- to be blocked eventually and GHC knows to kill it rather than keep burning
-- CPU transistors and other circuits.
--
main = do
  atomically $ do
    a  <- newEmptyTMVar
    b  <- newEmptyTMVar
    _  <- putTMVar a 1 -- <1>
    _  <- putTMVar b 3 -- <2>
    _a <- takeTMVar a
    _b <- takeTMVar b
    return (_a, _b)

-- This STM transaction succeeds when and only when both TMVars are full;
-- otherwise it is blocked. This explains why `retry` must abandon the whole
-- transaction if the first takeMVar succeeds but the second one retries, we do
-- not want the effect of the first takeTMVar to take place.
--
-- This example is difficult to program with MVar because taking a single MVar
-- is a side effect that is visible to the rest of the program, and hence
-- cannot be easily undone if the other MVar is empty. One way to implement it
-- is with a third MVar acting as a lock to control access to the other two,
-- but then of course all other clients have to be aware of the locking
-- protocol.
--

