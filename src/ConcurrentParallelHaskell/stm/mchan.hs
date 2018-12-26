import Control.Concurrent
import Control.Concurrent.STM
import Data.Functor (void)
import Control.Monad.IO.Class (liftIO)

-- Sample program to buffered, multi-item, multicast channels, in which items
-- written to the channel (writeMChan) are buffered internally and received
-- once by each read-port created from the channel.
--

type Chain a = TVar (Item a)
data Item a = Empty | Full a (Chain a)
type MChan a = TVar (Chain a)
type Port a = TVar (Chain a)

-- create a first multi-channel
newMChan :: STM (MChan a)
newMChan = do
  c <- newTVar Empty
  newTVar c

newPort :: MChan a -> STM (Port a)
newPort mc = do
  c <- readTVar mc
  newTVar c

-- write an item to the channel
writeMChan :: MChan a -> a -> STM ()
writeMChan mc i = do
  c  <- readTVar mc
  c' <- newTVar Empty
  _ <- writeTVar c (Full i c')
  writeTVar mc c'

-- read the next buffered item
readPort :: MChan a -> STM a
readPort p = do
  c <- readTVar p
  i <- readTVar c
  case i of
      Empty -> retry
      Full v c' -> do
        _ <- writeTVar p c'
        return v

-- demonstrate reading and writing
contend :: IO (Int, Int, Int, Int)
contend = do
  putStrLn "starting the contention..."
  result <- atomically $ do
    c <- newMChan
    p1 <- newPort c
    p2 <- newPort c
    p3 <- newPort c
    p4 <- newPort c
    writeMChan c 1
    writeMChan c 2
    writeMChan c 3
    writeMChan c 4
    b <- readPort p1
    a <- readPort p2
    e <- readPort p3
    d <- readPort p4
    return (a,b,d,e)
    -- return (sum [a,b,d,e])
  putStrLn $ "ended the contention:" ++ show result
  return result

main :: IO ()
main = do
  putStrLn ("Begin")
  r <- contend
  putStrLn (show r)
  return ()

-- According to the original paper "Composable memory transactions"
-- The implementation is very simple, it ensure sthat each item written into
-- the MChan is delivered to every Port; it allows multiple writers (their
-- writes are interleaved); it allows multiple reads on each port (data read
-- byone is not seen by the other reads on that port); and when a port is
-- discarded, the garbage collector recovers the buffered data.
--
-- More complicated variants are simple to program. For example, suppose we
-- wanted to ensure that the writer could get no more than N items ahead of the
-- most advanced reader. One way to do this would be for the writer to include
-- a serially-increasing Int in each Item, and have a shared TVar holding the
-- maximum serial number read so far by any reader. It is simple for the
-- readers to keep this up to date, and for the writer to consult it before
-- adding another item.
--
--


