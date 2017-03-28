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

## Non-strict evaluation changes what we can do

Let's look at examples of non-strictness and what it enables.
```haskell
Prelude> let mm = [1,2,3]
Prelude> tail mm
[2,3]
```
That works in either strict or non-strict languages because there is nothing
there that cannot be evaluated. However, if we keep in mind that `undefined` as
an instance of `bottom` then it will throw an error when forced:

```haskell
Prelude> undefined
*** Exception: Prelude.undefined
```

Below is an example of what i mean by being _forced_ ...
```haskell

Prelude> let mm = [undefined, 1,2]
Prelude> tail mm
[1,2]
Prelude> head mm
*** Exception: Prelude.undefined
```

A strict language would have crashed on construction of `mm` due to the
presence of bottom. This is because strict languages eagerly evaluate all
expressions as soon as they are constructed. In Haskell, however, non-strict
evaluation means that bottom values won't be evaluated unless it is needed for
some reason.

Here's a interesting expression which i thought would render the same but turns
out it wasn't. At first glance, it seems like list-generator functions like
`..` are in-built functions which aren't evaluated at declaration time and when
compared to literal list construction, the contrast is clear.

The interesting thing here is that even though we constructed `[1,2,3,4]` i
would expect that GHC should already know that its a `[Integer]` but turns out
its not evaluated but when i declared it for what it really is, we can see that
the entire expression is unfolded and revealed.

```haskell

Prelude> let listOfLiterals = [1..4]::[Integer]
Prelude> :sprint listOfLiterals
listOfLiterals = _
...
Prelude> let listOfLiterals = [1,2,3,4]::[Integer]
Prelude> :sprint listOfLiterals
listOfLiterals = [1,2,3,4]
Prelude>
...
Prelude> let listOfLiterals = [1,2,3,4]
Prelude> :sprint listOfLiterals
listOfLiterals = _

```

## Refutable and Irrefutable patterns

When we are talking about pattern matching, it is important to be aware that
there are refutable and irrefutable patterns. An irrefutable pattern is one
which will never fail to match. A refutable pattern is one which has potential
failures. Often, the problem is one of specificity.

```haskell
refutable :: Bool -> Bool
refutable True= False
refutable False = True

irrefutable :: Bool -> Bool
irrefutable x = not x

oneOfEach :: Bool -> Bool
oneOfEach True = False
oneOfEach _ = True
```

In the book, the authors cautioned us to bear in mind that the pattern is
refutable or not, not the function itself. The function `refutable` is
refutable because each case is refutable; each case could be given an input
that fails to match. In contrast, irrefutable has an irrefutable pattern; that
is , its pattern does not rely on matching with a specific value.

## What subverts or prevents sharing 

Sometimes we don't want sharing. Sometimes we want to know why sharing didn't
happen when we did want it. Understanding what kinds of things prevent sharing
is therefore useful. 

Inlining expressions where they get used prevents sharing because it creates
independent thunks that will get computed separately. Being a function with
explicit, named arguments also prevents sharing. Haskell is not fully lazy; it
is merely non-strict, so it is not required to remember the result of every
function application for a given set of arguments, nor would it be desirable
given memory constraints.

Implicit parameters are implemented similarly to typeclass constraints and have
the same effect on sharing. Sharing doesn't work in the presence of constraints
(typeclasses or implicit parameters) because typeclass constraints and
implicit parameters decay into function arguments when the compiler simplifies
the code. Values of a concrete, constant type can be shared, once evaluated.
Polymorphic values may be evaluated once but still not shared because
underneath, they continue to be functions awaiting application.

## Preventing sharing on purpose

When do we want to prevent sharing? When we don't want a large datum hanging
out in memory that was calculated to provide a much smaller answer.

## Forcing sharing 

You can force sharing by giving your expression a name. The most common way of
doing this is with `let`.
```haskell
-- calculates 1 + 1 twice
(1+1) * (1+1)

-- shares 1 + 1 result under 'x'
let x = 1 + 1 in x * x
```
With that in mind, if you take a look at the forever function in Control.Monad,
you might see something a litte mysterious looking:

```haskell
forever :: (Monad m) => m a -> m b
forever a = let a' >> a' in a'
```

Why the let expression? well, we want sharing here so that running a monadic
action indefinitely doesn't leak memory. The sharing here causes GHC to
overwrite the thunk as it runs each step in the evaluation, which is quite
handy. Otherwise, it would keep constructing new thunks indefinitely and that
would be very unfortunate.

