

What should distributed programming in Haskell look like from the programmer's
perspective ? Should it look like __Concurrent Haskell__, with `forkIO`,
`MVar` and _STM_? In fact, there are good reasons to treat distributed
computation very differently from computation on a shared-memory multicore:

- There is a realistic possibility of partial hardware failure: that is, some
  of the machines involved in a computation may go down while others continue
  to run. Indeed, given a large enough cluster of machines, having nodes go
  down becomes the norm. In would be unacceptable to simply abort the entire
  program in this case. Recovery is likely to be application-specific, so it
  makes sense to make failure visible to the programmer and let him handle it
  in a appropriate way for his application.

- Communication time becomes significant. This follows from the fallacical
  assumption that latency does not exist in the network because it most
  certainly does ☺ . In the shared memory setting, it is convenient and
  practical to allow unrestricted sharing. This is because, for example,
  passing a pointer to a larger data structure from one thread to another has
  no cost (beyond the costs imposed by the hardware and the runtime memory
  manager, but again its convenient and practical to ignore these). In a
  distributed setting, however, communication can be costly, and sharing a data
  structure between threads is something the programming will want to think
  about and explicitly control.

- In a distributed setting, it becomes far more difficult to provide any global
  consistency guarantees of the kind that, for example, _STM_ provides in the
  shared-memory setting. Achieving a consistent view of the state of the system
  becomes a very hard problem indeed. There are algorithms for achieving
  agreement between nodes in a distributed system, but the exact nature of the
  consistency requirements depend on the application so we do not want to build
  a particular algorithm into the system.

For these reasons, the Haskell developers decided that the model for
distributed programming should be based on explicit message passing and not
the `MVar` and _STM_ models that we provide for shared-memory concurrency.
Think of it as having `TChan` be the basic primitive available for
communication. Its possible to build higher-level abstractions on top of the
explicit message-passing layer, just we built higher-level abstractions on
top of _STM_ and `MVar`.

# Distributed Concurrency or Parallelism ?

It's a little unfortunate that we have to resort to a nondeterministic
programming model to achieve parallelism just because we want to exploit
multiple machines. There are efforts under way ot build deterministic
programming models atop the `distributed-process` framework, although at the
time of writing these projects are too experimental to include in this book.


# Processes and the Process Monad

Let's get our terminology correct. A distributed program consists of a set of
processes that may communicate with one another by sending and receiving
messages. A process is like a thread. Processes run concurrently with one
another, and every process has a unique `ProcessId`. There are a couple of
important differences between threads and processes, however:

- Threads are always created on the current node, whereas a process can be
  created on a remote node

- Processes run in the `Process` monad, rather than the `IO` monad. Process is
  an instance of `MonadIO` so you can perform IO operations in Process by
  wrapping them in `liftIO`. All message passing operations are in Process, so
  only processes, not threads, can engage in message passing.


