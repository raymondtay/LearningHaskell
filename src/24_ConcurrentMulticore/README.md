# Concurrent Programming with Threads

In Haskell, a thread is an `IO` action that executes independently from other
threads. To create a thread, we import the `Control.Concurrent` module and use
the `forkIO` function.

# Simple Communication between Threads

We often need to have threads actively communicate with each other. Haskell
provides a synchronizing variable type, the `MVar`, which we can use to create
this capability for ourselves.

If we try to put a value into an `MVar` that is already full, our thread is put
to sleep until another thread takes the value out. Similarly, if we try to take
a value from an empty `MVar`, our thread is put to sleep until some other
thread puts a value in.

# MVar and Chan are Nonstrict

Like most Haskell container types, both `MVar` and `Chan` are nonstrict:
neither evaluates its contents. We mention this because its a common
blindspot.

# Deadlock

In a deadlock situation, two or more threads get stuck forever in a clash over
access to shared resources.One classic way to make a multithreaded program
deadlock is to forget the order in which we must acquire locks. This kind of
bug is so common, it has a name : lock order inversion. While Haskell does not
provide locks, the `MVar` type is prone to the order inversion problem.

# Choosing the Right Runtime

The decision of which runtime to use is not completely clear cut. While the
threaded runtime can use multiple cores, it has a cost: threads and sharing
data between them are more expensive than with the nonthreaded runtime.

Furthermore, the garbage collector used by GHC as of version 6.8.3 is
single-threaded: it pauses all other threads while it runs and executes on one
core. This limits the performance improvement we ca hope to see from using
multiple cores. In many real-world concurrent programs, an individual thread
will spend most of its time waiting for a network request or response. In these
cases, if a single Haskell program serves tens of thousands of concurrent
clients, the lower overhead of the nonthreaded runtime maybe helpful. for
example, instead of having a single server program use the threaded runtime on
four cores, we might see better performance if we design our server so that we
can run four copies of it simultaneously and use the nonthreaded runtime.

# Knowing What to Evaluate in Parallel

The key to getting decent performance out of parallel Haskell code is to find
meaningful chunks of work to perform in parallel. Nonstrict evaluation can get
in the way of this, which is why we use the "force" function in our parallel
sort.
