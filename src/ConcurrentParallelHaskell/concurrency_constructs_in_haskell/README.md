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

## Asynchronous Exceptions : Discussion

The problem domain of exceptions and the manner in which they can occur is
tricky and has a lot of subtle details - such is life when dealing with
exceptions that can strike at any moment. The abstractions can be hard to get
right, but it is worth reminding ourselves that dealing with asynchronous
exceptions at this level is something that Haskell programmers rarely have to
do, for a couple of reasons.

- All non-IO haskell code is automatically safe by construction. This is the
  one factor that makes asynchronous exceptions feasible.

- We can use the abstractions provided, such as `bracket`, to acquire and
  release resources. These abstractions have asynchronous-exception safety
  built in. Similarly, when working with `MVar`s, the `modifyMVar` family of
  opereations provides built-in safety.

In the author's own words: We find that making most `IO` monad code safe is
straightforward, but for those cases where things get a bit complicated, a
couple of techniques can simplify matters:

- Large chunks of heavily stateful code can be wrapped in a `mask`, which drops
  into polling mode for asynchronous exceptions. This is much easier to work
  with. The problem then boils down to finding the interruptible operations and
  ensuring that exceptions raised by those will not cause problems. The GHC I/O
  library uses this technique: every `Handle` operation runs entirely inside
  `mask`.

- Using software transactional memory instead of `MVar` or other state
  representations can sweep away all the complexity in one go. STM allows us to
  combine multiple operations in a single atomic unit, which means we don't
  have to worry about restoring state if an exception strikes in the middle.

In exchange for asynchronous exception safety, Haskell's approach to
asynchronous exceptions confers some important benefits:

- Many exceptional conditions map naturally onto asynchronous exceptions. For
  example, stack overflow and user interrupt at mapped to asynchronous
  exceptions in Haskell. Hence, Ctrl+C not only aborts the program but also
  does so cleanly, running all the exception handlers. Haskell programmers dont
  have to do anything to enable this behavior.

- Computation can always be interrupted, even if it is third-party library
  code; with the exception to this is foreign functions.

- Threads never just die in Haskell. It is guaranteed that a thread always gets
  a chance to clean up and run its exception handlers.


