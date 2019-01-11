
# Performance

As with most abstractions, STM has a runtime cost. If we understand the cost
model, we can avoid writing cod that hits the bad cases. Here's an information
description of the implementation of STM, with enough detail that the reader
can understand the cost model.

An STM transaction works by accumulating a log of `readTVar` and `writeTVar`
operations that have happened so far during the transaction. The log is used in
three ways:

- By storing `writeTVar` operations in the log rather than applying them to
  main memory immediately, discarding the effects of a transaciton is easy; we
  just throw away the log. Hence, aborting a transaction has a fixed small
  cost.

- Each `readTVar` must traverse the log to check whether the `TVar` was written
  by an earlier `writeTVar`. Hence, `readTVar` is an O(n) operation in the
  length of the log.

- Because the log contains a record of all the `readTVar` operations, it can be
  used to discover the full set of TVars read during the transaction, which we
  need to know in order to implement `retry`.

When a transaction reaches the end, the STM implementation compares the log
against the contents of memory. If the current contents of memory match the
values read by `readTVar`, the effects of the transaction are committed to
memory, and if not, the log is discarded and the transaction runs again from
the beginnging. This process takes place atomically by locking all the `TVar`s
involved in the transaction for the duration. The STM implementation in GHC
does not use global locks; only the `TVar`s involved in the transaction are
locked during commit, so transactions operating on disjoint sets of `TVar`s can
proceed without interference.

There are two important rules of thumb;

- Never read an unbounded number of TVars in a single transaction because the
  O(n) perform of `readTVar` then gives O(n²) for the whole transaction.

- Try to avoid expensive evaluation inside a transaction because this will
  cause the transaction to take a long time, increasing the chance that another
  transaction will modify one or more of the same TVars, causing the current
  transaction to be re-executed. In the worst case, a long running transaction
  re-executes indefinitely because it is repeatedly aborted by shorter
  transactions.

The `retry` operation uses the transaction log to find out which `TVar`s were
accessed by the transaction, because changes to any of these TVars must trigger
a rerun of the current transaction. Hence, each `TVar` has a _watch list_ of
threads that should be woken up if the `TVar` is modified, and `retry` adds the
current thread to the watch list of all the `TVars` read during the current
transaction. When a transaction is committed, if any of the modified `TVars`
has a watch list, then the threads on the list are all woken up.

One other thing to watch out for is composing too many blocking operations
together. Here's an example to illustrate the general problem; if we wanted to
wait for a list of `TMVars` to become full, we might be tempted to do this:

```haskell
atomically $ mapM takeTMVar ts
```

imagie that the TMVars all started empty and became full one at a time in the
same order as the list ts. EAch time a new TMVar becomes full, the transaction
wakes up and runs again, going to sleep at the next empty TMVar. We'll run the
transaction from the start, once for every element of ts, so the whole
operation is O(n²). If instead of the former we had taken this particular route 

```haskell
mapM (atomically . takeTMVar) ts
```


