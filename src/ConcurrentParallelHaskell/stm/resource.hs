import Control.Concurrent
import Control.Concurrent.STM
import Data.Functor (void)

-- suppose we wish to implement a resource manager, which holds an
-- integer-valued resource. The call `getR r n` should acquire `n` units of
-- resource `r`, blocking if `r` holds insufficient resource: the call `putR r
-- n` should return `n` units of resource to `r`.
--

type Resource = TVar Int -- currently available resource is held in a transaction variable of type TVar Int.

putR :: Resource -> Int -> STM ()
putR r i = do
  v <- readTVar r
  writeTVar r (v+i)

-- getR looks at the current resources available and if it falls below
-- the requested resource, it would retry again and once its satisfied it would
-- basically store the new balance (after offsetting appropriately)
--
getR :: Resource -> Int -> STM ()
getR r i = do
  v <- readTVar r
  if (v < i) then retry else writeTVar r (v - i) -- "retry" blocks the current thread till the TVars are updated.

sequentialComposition :: IO ()
sequentialComposition =
  atomically $ do
    r <- newTVar 3
    getR r 1
    getR r 2

-- s1 `orElse` s2 first runs s1; if it retries, then s1 is abandoned and s2 is
-- executed instead. If s2 retries then the entire call is retried - but it
-- waits on the variables read by either of the two nested transactions.
--
eitherOne :: IO ()
eitherOne = 
  atomically $ do
    r <- newTVar 3
    (getR r 1) `orElse` (getR r 2)

eitherOneA :: IO Bool 
eitherOneA = 
  atomically $ do
    r <- newTVar 3
    (nonblockGetR r 1) `orElse` (nonblockGetR r 2)

eitherOneB :: IO () 
eitherOneB = 
  atomically $ do
    r <- newTVar 3
    (blockGetR r 1) `orElse` (blockGetR r 2)

-- The orElse function obeys useful laws: it is associative and has unit
-- `retry`.
--
nonblockGetR :: Resource -> Int -> STM Bool
nonblockGetR r i = do
  _ <- getR r i
  return True `orElse` return False

blockGetR :: Resource -> Int -> STM ()
blockGetR r i = do
  s <- nonblockGetR r i
  if s then return () else retry

main :: IO ()
main = do
  _ <- putStrLn "Beginning now..."
  void eitherOne
  void eitherOneA
  void eitherOneB
  -- void sequentialComposition
  putStrLn "Ended."

-- main = do
--   r <- newTVar 0
--   atomically (putR r 4)

-- Like all things in Haskell, STM would not be useful if it was not able to
-- handle exceptions and in this case, STM monad behaves like the IO monad. The
-- question is: how should transactions and exceptions interact ? For example,
-- what should the following do?
--
-- atomically $ do
--  n <- readTVar v_n
--  lim <- readTVar v_lim
--  _ <- writeTVar v_n (n + 1)
--  if n > lim then throw (AssertionFailed "Urgh")
--  else if (n == lim) then retry else return ()
--
--  blah blah blah
--



