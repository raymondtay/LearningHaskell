
import Control.Monad           (forM, void)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Chan

-- Simple demonstration of how a channel can be written to and read from
-- by using facilities from the [[Control.Concurrent.Chan]] package.
simpleDemonstration :: IO (Chan Int)
simpleDemonstration = do
  c <- newChan 
  _ <- forM [1..10] (\e -> forkIO $ writeChan c e)
  loop c
  return c
    where
      loop ch = do
        a <- readChan ch
        putStrLn $ "Saw this element:" ++ show a
        loop ch

-- Thinking out louad ... 
-- Concurrency in Haskell like any other programming language be it Java, C++
-- is tricky and the static typing system does little to help in detecting
-- concurrency problems.
--
type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

-- A stream represents the sequence of values currently stored in the
-- channel.Each element is an MVar containing an Item, which contains the value
-- and the rest of the Stream. The end of the Stream is represented by an empty
-- MVar called the hole, into which the next value to be written to the channel
-- will be placed.
--
-- The channel needs to track both ends of the Stream, because values read from
-- the channel are taken from the beginning, and values written are added to th
-- eend. Hence, a channel consists of two pointers called the "read" and the
-- "write" pointer respectively, both represented by MVars.
--

data C a = C (MVar (Stream a)) (MVar (Stream a)) -- C is aka Channel

-- At the beginning both read and write "pointers" are init to the same "hole"
newC :: IO (C a)
newC = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (C readVar writeVar)

readC :: C a -> IO a
readC (C readVar _) = do
  stream <- takeMVar readVar
  Item value tail <- readMVar stream
  -- Item value tail <- takeMVar stream this is largely wrong when duplicated
  -- channels are allowed because the duplicated channel
  putMVar readVar tail
  return value

writeC :: C a -> a -> IO ()
writeC (C _ writeVar) value = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item value newHole)
  putMVar writeVar newHole

-- The new channel is created empty and inherits the write pointer
-- of the channel its duplicating from. Subsequent writes to either Channel
-- are read from both; that is, reading an item from one channel does
-- not remove it from the other.
dupC :: C a -> IO (C a)
dupC (C _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (C newReadVar writeVar)

-- This doesn't work quite exactly because it cannot account for the situation
-- when the channel is empty
unGetChan :: C a -> a -> IO ()
unGetChan (C readVar _) value = do
  newRead <- newEmptyMVar
  currentRead <- takeMVar readVar
  putMVar newRead (Item value currentRead)
  putMVar readVar newRead

main :: IO ()
main = do
  c1 <- newC
  writeC c1 1
  writeC c1 2
  writeC c1 3
  -- 
  x <- readC c1
  putStrLn $ "Original channel ele: " ++ show x
  y <- readC c1
  putStrLn $ "Original channel ele: " ++ show y
  z <- readC c1
  putStrLn $ "Original channel ele: " ++ show z
  -- 
  c2 <- dupC c1

  writeC c1 1
  writeC c1 2
  writeC c1 3
 
  a <- readC c2
  putStrLn $ "Duplicate channel ele: " ++ show a
  b <- readC c2
  putStrLn $ "Duplicate channel ele: " ++ show b
  c <- readC c2
  putStrLn $ "Duplicate channel ele: " ++ show c
  putStrLn "done"


