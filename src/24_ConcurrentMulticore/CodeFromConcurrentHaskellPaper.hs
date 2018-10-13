import Control.Concurrent
type CVar a = (MVar a, -- producer -> consumer
               MVar () -- consumer -> producer
              )

-- from the paper "Concurrent Haskell", Section 3
-- A good way to understand a concurrency construct is by means of examples.
-- The following describes a few ways to use MVars: using standard examples
-- (such as buffering) allos easy comparison with the literature.
--
-- The first example is usuall a memory cell but of course an MVar implements
-- that directly. Another common exampleis a semaphore but an MVar implements
-- that directly too.
--
-- A buffer variable |||
--
-- An MVar can very nearly be used to mediate a producer/consumer connection:
-- the producer puts items into the MVar and the consumer takes them out. The
-- fly in the ointment is, of course, that there is nothing to stop the produce
-- over running and writing a second value before the consumer has removed the
-- first.
--
-- This problem is easily solved by using a second MVar to handle
-- acknowledgements from the consumer to the producer. We call the resulting
-- abstraction a CVar (short for channel variable)

putCVar :: CVar a -> a -> IO ()
putCVar (data_var, ack_var) val = takeMVar ack_var >> putMVar data_var val


getCVar :: CVar a -> IO a
getCVar (data_var, ack_var) =
  takeMVar data_var >>= \val ->
    putMVar ack_var () >> return val


newCVar :: IO (CVar a)
newCVar =
  newEmptyMVar >>= \data_var -> 
  newEmptyMVar >>= \ack_var -> 
    putMVar ack_var () >>
    return (data_var, ack_var)

-- A buffered Channel ||| 
--
-- A CVar can contain but a single value. Next, we show how to implement a
-- channel with unbounded buffering, along with some variants, its interface is
-- as follows:
--

data Item a = Item a (Stream a)
type Stream a = MVar (Item a)
type Channel a = (MVar (Stream a), -- Read
                  MVar (Stream a)  -- Write
                 )
-- A stream can therefore be thought of as a list, consisting alternating Items
-- and full MVars, terminated with a "hole" consisting of an empty MVar. The
-- write end of the channel points to this hole.
-- Creating a new channel is now just a matter of creating the read and write
-- MVars, plus one (empty) MVar for the stream itself.
--
-- The type signature of "Channel" might suggest that the "stream" value in the
-- LHS and the RHS might be different and that's a common misconception to
-- think that the type signature might seem to suggest that the values are
-- different. And honestly, who can blame you? For example, if i saw a tuple
-- (1,1) which is of type Num a => (a, a) one might conclude there are 2 items
--
-- The channel should permit multiple processes to write to it, and read from
-- it, safely.
--
newChan :: IO (Channel a)
newChan =
  newEmptyMVar >>= \read ->
    newEmptyMVar >>= \write ->
      newEmptyMVar >>= \hole ->
        putMVar read hole >>
          putMVar write hole >>
            return (read, write)

-- Putting into the channel entails creating a new empty Stream to become the
-- hole, extracting the old hole and replacing it with the new hole and then
-- putting the "Item" in the old hole.
putChan :: Channel a -> a -> IO ()
putChan (read,write) val =
  newEmptyMVar >>= \new_hole ->
    takeMVar write >>= \old_hole ->
      putMVar write new_hole >>
        putMVar old_hole (Item val new_hole)

getChan :: Channel a -> IO a
getChan (read, write) =
  takeMVar read >>= \cts -> -- reveals the "Stream"
    takeMVar cts >>= \(Item val new) -> -- reveals the "head" of the Stream
      putMVar read new >> return val


-- If this was a multi-cast channel, in which there are multiple readers, each
-- of which should see all the values written to the channel. All that is
-- required is to add a new operation "dupChan" and the idea is that the
-- channel returned by "dupChan" can be read independently of the original, and
-- sees all (and only) the data written to the channel after the "dupChan"
-- call. The implementation is simple, since it amounts to setting up a
-- separate read pointer, initialized to the current write pointer.
--

dupChan :: Channel a -> IO (Channel a)
dupChan (read, write) =
  newEmptyMVar >>= \new_read -> 
    takeMVar write >>= \hole ->
      putMVar write hole >>
        putMVar new_read hole >>
          return (new_read, write)

-- The inverse of "getChan" which is to the item back at the front of the FIFO
-- ADT.
unGetChan :: Channel a -> a -> IO ()
unGetChan (read, write) val =
  newEmptyMVar >>= \new_hole ->
    takeMVar read >>= \cts ->
      putMVar new_hole (Item val cts) >>
      putMVar read new_hole 


{- |
  Concurrent Haskell runs as a single Unix process, performing its own scheduling internally.
  Each user of forkIO creates a new process, with its own (heap-allocated) stack. The scheduler
  can be told to run either pre-emptively (time-slicing among runnable processes) or non-pre-emptively
  (running each process until it blocks). The scheduler only switches processes at well-defined
  points at the beginning of basic blocks; at those points there are no half-modified heap
  objects, and the liveness of all registers (notabley pointers) is known.

  A thunk is represented by a heap-allocated object containing a code pointer and the values of
  the thunk's free variables. A thunk is evaluated by loading a pointer to it into a defined
  register and jumping to its code. When a process begins the evaluation of a thunk, it replaces
  the thunk's code pointer with a special "under-evaluation" code pointer. 

  Accordingly, any other process that attempts to evaluate that thunk while it is under evaluation 
  will automatically jump to the "under-evaluation" code, which queues the process on the thunk.
  When the original process completes evaluation of the thunk it overwrites the thunk with its
  final value, and fress any blocked processes.


-}
