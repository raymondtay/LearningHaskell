
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

