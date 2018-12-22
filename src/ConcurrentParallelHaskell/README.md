
The `seq` function only evaluates its argument only as far as its first
constructor, does not evaluate any more of its structure. Haskellers say that
it evaluates to its _weak-head normal form_ (aka _WHNF_ in regular parlance).

Remember for the experiments described in this book, it is apparently important
to be able to provide type annotations to the unevaluated expressions.

# Notes

An important principle when it comes to parallelizing code: Try to avoid
partitioning the work into a small, fixed nujmber of chunks. There are 2
readsons:
- In practices, chunks rarely contain an equal amount of work, so there will be
  some imbalance leadning to a loss of speedup
- The parallelism we can achieve is limited to the number of chunks. In our
  examples, even if the workloads were even, we could never achieve a speedup
  of more than than 2, regardless of how many cores we use.


GHC does not force us to use a fixed number of rpar calls; we can call it as
many times as we like, and the system will automatically distribute the
parallel work among the available cores. If the work is divided into smaller
chunks, then the system will be able to keep all the cores busy for longer. 

A Fixed division of work is often called _static paritioning_, whereas
distributing smaller units of work among processors at runtime is called
_dynamic partioning_. GHC already provides the mehcniams for dynamic
partitioning: we just have to supply it with enough tasks by calling rpar often
enough so that it can do its job and balance the work evently.




The argument to `rpar` is called a _spark_. The runtime collects sparks in a
pool and uses this as a source of work when there are spare processors
available, using a technique called _work stealing_. Sparks may be evaluated at
some point in the future, or they might not - it all depends on whether there
is a spare core available. Sparks are very cheap to create: `rpar` essentially
just writes a pointer to the expression into an array. See [[sudoku3.hs]] on
how to use dynamic partitioning.


Evaluation strategies, or simply Strategies, are a means for modularising
parallel code by separating the algorithm from the parallelism. Sometimes, they
require you to rewrite your algorithm, but once you do so you will be able to
parallelise in different ways just by substituting a new Strategy.

### Fairness

Concurrent programs should be executed with some degree of _fairness_. At the
very least, no thead should be starved of CPU time indefinitely, and ideally
each thread should be given an equal share of the CPU.

GHC uses a simple _round-robin_ scheduler. It does guarantee that no thread is
starved indefinitely, although it does not ensure that every thread gets an
exactly equal share of the CPU. In practice, the schedule is reasonably fair in
this respect. The `MVar` implementation also provides an important fairness
guarantee:

```
No thread can be blocked indefinitely on an MVar unless another thread holds
that MVar indefinitely.
```

Take care to note that it is not enough to merely wake up the blocked thread
because another thread might run first and take (respectively put) the `MVar`,
causing the newly worked thread to go to the back of the queue again, which
would invalidate the fairness guarantee.

The implementation must therefore wake up the blocked thread and perform the
blocked operation in a single atomic step, which is exactly what GHC does.

A consequence of the fairness implementation is that, when multiple threads are
blocked in `takeMVar` and another thread does a `putMVar`, only one of the
blocked threads becomes unblocked. This "single wakeup" property is a
particularly important performance characteristic when a largr number of
threads are contending for a single `MVar`. As we shall see later, it is the
fairness guarantee - together with the single wakeup property - that keeps
MVars from being completely subsumed by software transactional memory.

# STM

STM operations are _composable_ i.e. any operation of type `STM a` can be
composed with others to form larger atomic transaction. For this reason, STM
operations are usually provided without the `atomically` wrapper so that
clients can compose them as necessary before finally wrapping the entire
operation in `atomically`.

```
Why is STM a different monad from IO? The STM implementation relies on being
able to roll back the effects of a transaction in the event of a conflict with
another transaction. A transaction can be rolled back only if we can track
exactly what effects it has, and this would not be possible if arbitrary I/O
were allowed inside a transaction - we might have performed some I/O that
cannot be undone, like making a noise or launching some missiles. For this
reason, the STM monad permits only side effects on TVars, and the STM
implementation tracks these effects to ensure the correct transaction
semantics.
```


