### The `MVar`

This is a fundamental building block that generalizes many different
communication and synchronization patterns, and over the next few sections we
shall see examples of these various use cases. To summarsize the main ways iin
which an `MVar` can be used:

- An `MVar` is a one-place channel, which means that it can be used for passing
  messages between threads, but it can hold at most one message at a time.

- An `MVar` is a container for shared mutable state. For example, a common
  design pattern in Concurrent Haskell, when several threads need read and
  write access to some state, is to represent the state value as an ordinary
  immutable Haskell data structure stored in an MVar. Modifying the state
  consists of taking the current value with `takeMVar` (which implicitly
  acquires a lock), and then placing a new value back in the `MVar` with
  `putMVar` (which implicitly releases the lock again).

  Sometimes the mutable state is not a Haskell data structure; it might be
  stored in C code or on the filesystem, for example. In such cases, we can use
  an MVar with a dummy value such as () to act as a lock on the external state,
  where `takeMVar` acquires the lock and `putMVar` releases it again.

- An `MVar` is a building block for constructing larger concurrent Data
  structures.


### Masking Asynchronous Exceptions

What's the purpose of masking asynchronous exceptions? The main issue here is
that when we have _allow_ asynchronous exceptions then there's a real
possibility that one might fire during the update of some shared state which
obviously is not a desirable situation. Therefore, we need a way to control the
delivery of these exception types in _critical sections_.

So, the above is the real reason why we need _masking_; in Haskell, the
combinator looks like:

```haskell
mask :: ((IO a) -> (IO a) -> IO b) -> IO b
```
This operation defers the delivery of asynchronous exceptions for the duration
of its argument.

