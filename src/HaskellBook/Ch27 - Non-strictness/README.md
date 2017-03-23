# Non-strictness

`Robert A. Heinlein` once said:

> Progress doesn't come from early risers - progress is made by lazy men looking
> for easier ways to do things.

Understanding `bottom`s is important in Haskell because non-strictness is
defined by the ability to evaluate expressions that have bindings which are
bottom in them, as long as the bottom itself is never forced. Bottoms also give
us a convenient method of observing evaluation in Haskell. By causing the
program to halt immediately with an error, bottom serves as our first means of
understanding non-strictness in Haskell.

## Standards and obligations

Technically, Haskell is only obligated to be non-strict, not lazy. A truly lazy
language memoizes, or holds in memory, the results of all the functions it does
evaluate, and outside of toy programs, this tends to use unacceptably large
amounts of memory. Implementations of Haskell, such as GHC Haskell, are only
obligated to be non-strict such that they have the same behavior w.r.t
`bottom`; they are not erquired to take a particular approach to how the
program executes or how efficiently it does so.
 
The essence of non-strictness is that you can have an expression which results
in a value, even if bottom or infinite data lurks within.


