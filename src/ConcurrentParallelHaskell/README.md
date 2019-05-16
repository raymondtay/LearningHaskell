
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

# Debugging notes

### Detecting Deadlock

The GHC runtime system can detect when a thread has become deadlocked and send
it the `BlockedIndefinitelyOnMVar` exception. How exactly does this work? In
GHC both threads and `MVar`s are objects on the heap, just like other data
values. As MVar that has blocked threads is represented by a heap object that
points to a list of the blocked thread. Heap objects are managed by the garbage
collector, which traverses the heap starting from the _roots_ to discover all
the live objects. The set of roots consists of the running threads and the
stack associated with teach of these threads. Any thread that is not reachable
from the roots is definitely deadlocked. The runtime system cannot ever find
these threads by following pointers, so they can never become runnable again.


Deadlock detection works using garbage collection, which is necessarily a
conservative approximation to the true future behavior of the program.


### Tuning Concurrent and Parallel Programs

- Avoid premature optimizations. Don't overoptimize code till you know that
  there's a problem. That said, this is not an excuse for writing awful code.
  For example, don't use wildly inappropriate data structures if using the
  right one is just a matter of importing a library. I like to "write code
  using efficiency in mind" : know the complexity of your algorithms, and if
  you find yourself using something worse than O(n log n), think about whether
  it might present a problem down the road. The mroe of this you do, the better
  your code will copy with larger and larger problems.

- Don't waste time optimizing code that doesn't contribute much to overall
  runtime. Profile your program so that you can focus your efforts on the
  important parts. GHC has a reasonable space and time profiler that should
  pointer at least where the inner loops of your code are. In concurrent
  programs, the problem can often be I/O or contention in which case using
  _ThreadScope_ together with _labelThread_ and _traceEvent_ can help track
  down the culprits.

### Thread Creation and MVar operations

# The Par Monad Compared to Strategies

As a general rule of thumb, if your algorithm naturally produces a lazy data
structure then writing a `Strategy` to evaluate it in parallel will probably
work well. If not then it can be more straightforward to use the `Par` monad to
express the parallelism.

The `runPar` funciton itself is relatively expensive, whereas `runEval` is
free. So when using the `Par` monad, you should usually try to thread the `Par`
monad around to all the places that need parallelism to avoid needing multiple
`runPar` calls. If this is inconvenient, then `Eval` or `Strategies` might be a
better choice. In particular, nested calls to `runPar` (Where a runPar is
evaluated during the course of executing another `Par` computation) usually
gives poor results.

Strategies allow a separation between algorithm and parallelism, which can
allow more reuse and a cleaner specification of parallelism. However, using a
parallel skeleton works with both approaches.

The `Par` monad has more overhead than the `Eval` monad. At the present time,
`Eval` tends to perform better at finer granularities, due to direct runtime
system support for sparks. At larger granularities, `Par` and `Eval` perform
approximately the same.

The `Par` monad is implemented entirely in a Haskell library (the monad-par
package) and is thus easily modified. There is a choice of scheduling
strategies.

The `Eval` monad has more diagnostics in _ThreadScope_. There are graphs that
show different aspects of sparks:
- creation rate
- conversion rate
- etc
The `Par` Monad is not currentlky integrated with _ThreadScope_.


