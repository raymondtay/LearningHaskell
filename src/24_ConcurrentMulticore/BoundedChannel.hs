-- | The `Chan` type is implemented using `MVars`. Use `MVars` to develop a
-- `BoundedChan` library.
-- Your `newBoundedChan` function should accept an Int parameter, limiting the
-- number of unread items that can be present in a `BoundedChan` at once. If
-- this limit is hit, a call to your `writeBoundedChan` function must block
-- until a reader uses readBoundedChan to consume a value.
--

module MyBoundedChannel where

import Control.Concurrent
import Control.Monad.IO.Class

type Bound = (MVar Int, -- current
              MVar Int  -- upper bound
             )
type BoundedChannel a = (MVar (Stream a), -- producer -> consumer
                         MVar (Stream a), -- consumer -> producer
                         Bound, -- maintains minimum number of unread items.
                         MVar Bool
                        )

data Item a = Item a (Stream a)
type Stream a = MVar (Item a)

-- When this channel is created, it will be allowed to be written since size <
-- capacity.
newBoundedChan :: Int -> IO (BoundedChannel a)
newBoundedChan size =
  newEmptyMVar >>= \read ->
    newEmptyMVar >>= \write ->
      newEmptyMVar >>= \hole ->
        newEmptyMVar >>= \canWrite ->
          newEmptyMVar >>= \sizeMVar ->
            newEmptyMVar >>= \current ->
              putMVar sizeMVar size >>
                putMVar read hole >>
                  putMVar write hole >>
                    putMVar current 0 >>
                      putMVar canWrite True >>
                    return (read, write, (current, sizeMVar), canWrite)

-- When a write is being conducted, a check is made to see if we are
-- approaching maximum capacity and if it is, then we will wait
-- but if not then we proceed to write to it.
--
writeBoundedChan :: BoundedChannel a -> a -> IO ()
writeBoundedChan (read, write, (a, b), canWrite) val =
  takeMVar a >>= \current ->
  readMVar b >>= \max ->
    case (current == max) of -- maximum capacity reached, so we wait
        True -> takeMVar canWrite >>= \w ->
                  newEmptyMVar >>= \new_hole ->
                    takeMVar write >>= \old_hole ->
                      putMVar write new_hole >>
                      putMVar a (current + 1) >>
                      putMVar old_hole (Item val new_hole)
        False -> newEmptyMVar >>= \new_hole ->
                   takeMVar write >>= \old_hole ->
                     putMVar write new_hole >>
                     putMVar a (current + 1) >>
                     putMVar old_hole (Item val new_hole)


readBoundedChan :: BoundedChannel a -> IO a
readBoundedChan (read, write, (a, b), canWrite) =
  takeMVar read >>= \cts ->
    takeMVar cts >>= \(Item val new) ->
      takeMVar a >>= \current ->
        putMVar a (current - 1) >>
        putMVar read new >> return val


readWhenEmpty :: IO ()
readWhenEmpty = do
  c <- newBoundedChan 1 :: IO (BoundedChannel Int)
  r <- readBoundedChan c
  putStrLn ("datum read from channel")
  putStrLn ("The result is: " ++ show r)

writeAllReadAll = do
  c <- newBoundedChan 3
  putStrLn ("Channel created")
  _ <- writeBoundedChan c 1
  _ <- writeBoundedChan c 2
  _ <- writeBoundedChan c 3
  putStrLn ("datum written to channel")
  r1 <- readBoundedChan c
  r2 <- readBoundedChan c
  r3 <- readBoundedChan c
  putStrLn ("datum read from channel")
  putStrLn ("The result is: " ++ show r1)
  putStrLn ("The result is: " ++ show r2)
  putStrLn ("The result is: " ++ show r3)

main :: IO ()
-- main = readWhenEmpty
main = writeAllReadAll


