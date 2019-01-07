
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
