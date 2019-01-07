import Control.Concurrent.STM hiding (TBQueue, newTBQueue, writeTBQueue, readTBQueue)


-- Fortunately, STM makes it quite straightforward to build a bounded channel.
-- All we need to do is keep track of the current capacity in the channel and
-- arrange that writing to the channel blocks if the channel is currently full.
-- This implementation is based on TQueue:
--

data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a]) 

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read <- newTVar []
  write <- newTVar []
  cap <- newTVar size
  return (TBQueue cap read write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue cap _read write) a = do
  available <- readTVar cap
  if available == 0 then retry 
                    else writeTVar cap (available - 1)
  listend <- readTVar write
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue cap read write) = do
  available <- readTVar cap
  writeTVar cap (available + 1)
  xs <- readTVar read
  case xs of
      (x:xs') -> do writeTVar read xs'
                    return x
      [] -> do ys <- readTVar write
               case ys of
                   [] -> retry
                   _  -> do let (z:zs) = reverse ys
                            writeTVar write []
                            writeTVar read xs
                            return z


-- The danger with bounded channels is that it is possible to write a program
-- with a lurking deadlock that is only discovered much later when the program
-- is running in production. This is because the vast majority of the time
-- writeTBQueue does not block, but once in a while probably under heavy load,
-- the channel fills up and writeTBQueue blocks. If the program depends on
-- writeTBQueue not blocking, it may deadlock. How might we get into this
-- situation ? It is the dining philosophers problem again.
--
-- The best advice is to test your code thoroughly with a buffere size of 1,
-- because that will tend to expose any deadlocks of this kind during testing.
-- Note that deadlocks will often be detected by the runtime system and result
-- in an exception rather than a hang.
--
--


