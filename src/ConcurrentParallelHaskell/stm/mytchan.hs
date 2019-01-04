{-# LANGUAGE ConstraintKinds #-}

import Control.Concurrent.STM hiding (TChan, newTChan, readTChan, writeTChan, unGetTChan)

data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

readTChan :: TChan a -> STM a
readTChan (TChan readVar _) = do
  listHead <- readTVar readVar
  head <- readTVar listHead
  case head of
      TNil -> retry
      TCons value tail -> do
        writeTVar readVar tail
        return value

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) a = do
  newListEnd <- newTVar TNil
  listEnd <- readTVar writeVar
  writeTVar writeVar newListEnd
  writeTVar listEnd (TCons a newListEnd)

-- All the operations are in the STM monad, so to use them they need to be
-- wrapped in atomically but they can also be composed, more about that later.
--
-- The TList type needs a TNil constructor to indicate an empty list; in the
-- MVar implementation, the empty list was represented implicitly by an empty
-- MVar.
--
-- Blocking in readTChan is implemented by a call to retry.
--
-- Nowhere did we have to worry about what happens when a read executes
-- concurrently with a write, because all the operations are atomic.
--
--

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan readVar _) a = do
  listHead <- readTVar readVar
  newHead  <- newTVar (TCons a listHead)
  writeTVar readVar newHead

-- the obvious implementation does the right thing here. Other operations that
-- were not possible with MVars are straightforward with STM; an example is
-- isEmptyTChan, the MVar version that suffers from the same problem as
-- unGetChan:
--
isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _write) = do
  listhead <- readTVar read
  head <- readTVar listhead
  case head of
      TNil -> return True
      TCons _ _ -> return False

simpleDemo :: STM (Int, Int)
simpleDemo = do
  chan <- newTChan
  _ <- writeTChan chan 1
  _ <- writeTChan chan 2
  _ <- writeTChan chan 3
  a <- readTChan chan
  b <- readTChan chan
  c <- readTChan chan
  return (a, b+c)

main = atomically $ do
  chan1 <- newTChan
  chan2 <- newTChan
  _ <- writeTChan chan2 4344 -- remember that readEither is left-biased.
  readEitherTChan chan1 chan2

-- composition of blocking operations
--
-- because blocking STM computations can be composed together, we can build
-- composite operations like readEitherTChan
--
-- readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
--
-- This functions reads a value from either of the two TChans passed as
-- arguments, or blocks if they are both empty. Its implementation should look
-- familiar as in takeEitherTMVar
--

readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
  fmap Left (readTChan a) `orElse` fmap Right (readTChan b)

-- simpleDemo2 :: Num (a, b) => Maybe (TChan a) -> STM (Either a b)
simpleDemo2 possiblyEmptyChan = do
  chan <- newTChan
  _ <- writeTChan chan 9
  return (fmap (readEitherTChan chan) possiblyEmptyChan)


