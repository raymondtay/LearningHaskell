
-- Here's a different queue data structure that supports O(1) enqueue and
-- dequeue operations. There is a folklore technique for representing a queue
-- that has the desired property: the idea is to represent a queue as two
-- lists, xs and ys, where the whole contents of the list is given by xs ++
-- reverse ys. That is, to take an element from the front we take it from xs,
-- and to add an element to the back we add it to the front of ys; both of
-- these operations are O(1). But what if xs is empty and we need to take an
-- element? In that case, we mnust recerse ys and let that become the new xs.
-- So while most of the time, taking an element from the front is O(1),
-- occasioanllay it is O(n). However, we know that each list element is
-- reversed only once, so one average the complexity of both enqueue and
-- dequeue is O(1).
--
-- By default, in the libraries there's no instance of MonadIO for STM so
-- i cannot use `liftIO` to perform some kind of debugging.


import Control.Monad.IO.Class
import GHC.Conc
import Data.Functor
import Control.Concurrent
import Control.Concurrent.STM hiding (TQueue, readTQueue, writeTQueue, newTQueue)

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
  read <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of 
      (x:xs') -> do writeTVar read xs'
                    return x
      [] -> do ys <- readTVar write
               case ys of
                   [] -> retry
                   _  -> do let (z:zs) = reverse ys
                            writeTVar write []
                            writeTVar read zs
                            return z


-- qread :: Show a => TQueue a -> STM a
qread queue = do
  r <- readTQueue queue
  -- putStrLn ("I got a : " ++ show r)
  return r

main = atomically $ do
  q <- newTQueue
  mapM_ (writeTQueue q) [1..1000]
  a <- readTQueue q
  b <- readTQueue q
  c <- readTQueue q
  -- unsafeIOToSTM $ qread q -- readTQueue q
  return (a, b, c)


