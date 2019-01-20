
# Programming with multiple threads

We have been discussing concurrency as a means to modularize programs with
multiple interactions. Concurrency can also be used to achieve true
parallelism.

Whe multiple cores are available, the GHC runtime system automatically migrates
threads between cores so that no cores are left idle. Its load balancing
algorithm isn't very sophisticated, though, so don't expect the scheduling
policy to be fair, although it does try to ensure that threads do not get
__starved__.


# Does parallelizing a program mean that there's a significant speed bump ?

Nope. It doesn't happen quite like that in reality. In the book's example
[[findseq.hs]] versus [[findpar.hs]] you would notice that there really is
indeed a lack of speedup in the _parallel_ version and the main reason for that
is because of overhead ...and the next question is "Where's the overhead then?"

When we examine the eventlog on one (or perhaps two) runs of the _parallelized_
program, a first suspicion is to check whether there's a possibility that we
might have caused too much parallelism such that the compute cores is spending
too much time creating threads of execution versus doing actual work (you can
confirm this if the productivity or output is too low).

This is commonly known as fine-grained parallelism and the immediate solution
to this is to turn it into coarse-grained parallelism and a way to achieve this
is to __chunk__ more work into each parallel thread.

However, the computation here in the [[findpar.hs]] is tree-shaped, so we
cannot just chunk more directories per executing thread per se; what would make
more sense is to place a threshold on the depth of the search and this makes
sense for algorithms that are of the nature of _divide-and-conquer_.

  - You should be aware that even though we made a claim that its tree-shaped,
    it doesn't necessarily mean that its a balanced tree-shaped sort of thing.
    That should be obvious because directories on _ANY_ filesystem can have any
    number of sub-directories (recursively speaking).

# The ParIO Monad

There is a version of the [[Par]] monad called [[ParIO]] provided by the module
`Control.Monad.Par.IO` with two important differences from [[Par]]:

- `IO` operations are allowed inside `ParIO`. To inject an `IO` operation into
  a `ParIO`n computation, use `liftIO` from the `MonadIO` class.

- For this reason, the pure `runPar` is not available for `ParIO`. Instead, a
  parallel computation is performed by the following:
  ```
  runParIO :: ParIO a -> IO a
  ```

Of course, unliked `Par`, `ParIO` computations are not guaranteed to be
deterministic. Nevertheless, the full power of the `Par` framework is
available: very light weight tasks, multicore scheduling, and the same dataflow
API based on `IVar`s. `ParIO` is ideal for parallel programming in the IO
monad, albeit with one caveat (see the code examples for this).
