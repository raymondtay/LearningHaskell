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

A strict language is evaluating each binding as it comes into scope, not when a
binding is used. The `seq` function has the type signature `seq :: a -> b -> b`
but what it really means is that whenever the second value of type 'b' needs to
be evaluated, then it would force the evaluation of the first value of type
'a'. Evaluatin in Haskell is demand-driven, we cannot guarantee that something
will ever be evaluated *period*. Instead, we have to create links between nodes
in the graph of expressions where forcing one expression will force yet another
expression.
 
# seq and weak head normal form

What `seq` does is evaluate your expression up to weak head normal form. WHNF
evaluation means it stops at the first data constructor or lambda. 

Another way we can talk about different evaluation strategies is by
distinguishing them on the basis of call by name, call by need and call by
value.

### Call by value

Argument expressions have been evaluated before entering a function. The
expressions that bindings reference are evaluated before creating the binding.
This is conveniently called strict. This is inside-out evalution.

### Call by name

Expressions can be arguments to a function without having been evaluated, or in
some cases, never being evaluated. You can create bindings to expressions
without evaluating them first. Non-strictness includes this evaluation
strategy. This is outside-in.

### Call by need

This is the same as call by name, but expressions are only evaluated once. This
only happens some of the time in GHC Haskell, usually when an expressions isn't
a lambda that takes arguments and also has a name. Results are typically shared
within that name only in GHC Haskell (that is, other implementations of Haskell
may choose to do things differently). This is also non-strict and outside-in.

