-- import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically, STM, TVar, retry, newTVar, readTVar, writeTVar)

newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
  v <- newTVar []
  return (TList v)

writeTList :: TList a -> a -> STM ()
writeTList (TList v) a = do
  list <- readTVar v
  writeTVar v (list ++ [a])

readTList :: TList a -> STM a
readTList (TList v) = do
  xs <- readTVar v
  case xs of
      [] -> retry
      (x:xs') -> do
        writeTVar v xs'
        return x

main = atomically $ do
  newlist <- newTList
  mapM_ (writeTList newlist) [1..5]
  a <- (readTList newlist)
  b <- (readTList newlist)
  return (a, b)

