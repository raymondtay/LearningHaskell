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


