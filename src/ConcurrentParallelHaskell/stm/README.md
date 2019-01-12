# The Basics

STM gives us a few simple, but powerful, tools with which we can address most
of these problems. We execute a block of actions as a transaction using the
`atomically` combinator. Once we enter the block, other threads cannot see any
modifications we make until we exit, nor can our thread see any changes made by
other threads. These two properties mean that our execution is _isolated_.

Upon exit from a transaction, exactly one of the following things will occur:

- If no thread concurrently modifies the same data as us, all of our
  modifications will simultaneously become visible to other threads;

- otherwise, our modifications are discarded without being performed, and our
  block of actions is automatically restarted.

This all or nothing nature of an `atomically` block is referred to as _atomic_,
hence the name of the combinator. If you have used databases that support
transactions, you should find that working with STM feels quite familiar.



STM provides a subtrate for concurrent programming that offers far richer
composition than has been available to date, and that it can be implemented in
a practical language.

We have used Haskell as a particularly suitable laboratory, but an obvious
question is this: to what extent can our results be carried back into the
mainstream world of imperative programming? We believe that the idea of using
constructs like `retry` and `orElse` can indeed be applied to other languages.
For instance, in C# one could indicate `retry` by raising a specific kind of
exception and then express `orElse` as a particular kind of exception handler.

# What can we not do with STM ?

STM offers a marked improvement over `MVar` with the following ways: composable
atomicity, composable blocking, and simpler error handling. Therefore, it is
reasonable to ask whether we need `MVar` at all, and whether there is anything
that is harder to accomplish with STM than with `MVar`. 

One unsurprising advantage of `MVar` is that it isfaster than STM. But even
though a straight forward comparison of, say, `takeMVar` against `atomically .
takeTMVar` will show that `takeMVar` is faster, we should not assume that using
`MVar` will always result in faster code. As we saw in the previous section, we
can build a channel using STM that outperforms the `MVar-based` version and
furthermore is composable.

In fact, MVar does have one other important advantage over STM, which we
mentioned earlier: fairness. When multiple threads block on an MVar, they are
guaranteed to be woken up in FIFO order, and no single thread can be blocked in
takeMVar indefinitely so long as there is a constant supply of putMVars.
In contrast, when multiple threads are blocked in STM transactions that depend
on a particular TVar, and the TVar is modified by another thread, its not
enough to just wake up one of the blocked transactions - the runtime must wake
them all.


The only way to implement fairness is to abandon composability. We can
implement a TMVar with the structure i suggested, but the operations must be in
the IO Monad, not the STM monad. The trick is to have the STM transaction
return an IO action that is executed after the STM transaction completed. I'll 


# What happens when we retry?

The `retry` function does not just make our code clearner - its underlying
behavior seems nearly magical. When we call it, it doesn't restart our
transaction immediately. Instead, it blocks our thread until one or more of the
variables that we touched before calling `retry` is changed by another thread.

```haskell
transfer :: Gold -> Balance -> Balance -> STM ()
transfer qty fromBal toBal = do
  from <- readTVar fromBal
  when (qty > from) $ retry
  writeTVar fromBal (from - qty)
  readTVar toBal >>= writeTVar toBal . (qty +)
```
for instance, if we invoke transfer with insufficient funds (see above),
`retry` will automatically _wait_ until our balance changes before it starts
the `atomically` block again. The same happens with our new `giveItem`
function: if the sender doesn't currently have the item in his inventory, the
thread will block until he does.


# I/O and STM

The STM monad forbids us from performing arbitrary I/O actions, because they
can break the guarantees of atomicity and isolation that the monad provides. Of
course, the need to perform I/O still arises - we just have to treat it very
carefully. Most oftne we will need to perform some I/O action as a result of a
decision we made inside an `atomically` block. In these cases, the right to do
is usually to return a piece of data from `atomically`, which will tell the
caller in the IO moand what to do next. We can ven return the action to
perform, since actions are first-class values

```haskell

someAction :: IO a
someAction = undefined

stmTransaction :: STM (IO a)
stmTransaction = return someAction

doSomething :: IO a
doSomething = join (atomically stmTransaction)
```

This code above ↑ deserves some attention and scrutiny because you can see one
can extract the transaction by applying the usual combinators i.e. `join`

Occasionally, we need to perform I/O operations from within STM. For instance,
reading immutable data from a file that must exist does not violate the STM
guarantees of atomicity or isolation and in these cases, we can use
unsafeIOToSTM to execute an IO action. For example,
```haskell
import GHC.Conc
unsafeIOToStm :: IO a -> STM a
```

The IO action that we execute must not start another atomically transaction.
If a thread tries to nest transactions, the runtime will throw an exception.
Since the type system cannot help us to ensure that our IO code is doing
something sensible, we wil lbe safest if we limit our use of unsafeIOToSTM as
much as possible.

For example, if the `mightRetry` block causes our transaction to restart, we
will call launchTorpedoes more than once.
```haskell

import GHC.Conc
import Control.Concurrent.STM

launchTorpedoes :: IO ()
launchTorpedoes = putStrLn "Torpedoes launched!"

doStuff :: STM ()
doStuff = do
  dummy <- newTVar 1
  unsafeIOToSTM $ putStrLn "doStuff"
  return ()

mightRetry :: STM a
mightRetry = retry

notActuallyAtomic :: STM a
notActuallyAtomic = do
  doStuff
  unsafeIOToSTM launchTorpedoes
  mightRetry

```

