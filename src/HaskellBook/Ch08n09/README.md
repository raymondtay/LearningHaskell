
# Recursion 

This topic is central to functional programming; probably even more so for a
programming language like Haskell. Because Haskell is built on pure lambda
calculus, recursion is implemented in the language through the Y, or
fixed-point combinator. You can read a very good explanation of that at
http://mvanier.livejournal.com/2897.html if you are interested in learning how
it works.

# Lists in Haskell

Haskell's evaluation is nonstrict, the list isn't constructed until is is
consumed - indeed,nothing is evaluated untill it must be. Until it's consumed
or you forced strictness in some way, there are a seires of placehodlers as
blueprint of the list that can be constructed when it's needed. 

Values in Haskell get reduced to weak head normal form  by default. By "normal
form" we mean that the expression is fully evaluated. Weak head normal form
means the expressions is only evluated as afar as it necessary to reach a data
constructor.

# Spines are evaluated independently of values

Values in Haskell get reduced to weak head normal form by default; by _normal
form_ we mean that the expression is fully evluated. Weak head normal form
means the expressions is only evaluated as far as is necessary to reach a data
constructor.

_Weak head normal form_ (WHNF) is a larger set and contains both the possibility that
the expressions is fully evaluated (normal form) and the possiblity that the
expression has been evaluated to the point of arriving at a data constructor or
lambda awaiting an argument. For an expression in weak head normal form,
further evaluation may be possible once another argument is provided. If no
further inputs are possible, then it is still in WHNF but also in normal form. 

```haskell

(1,2) -- WHNF and NF

(1, _ + _ ) -- WHNF but not NF. The (+) and its unknown arugments could be
-- evaluated.

(1, 1 + 1) -- WHNF but not NF. The 1 + 1 could be evaluated.

\x -> x * 10 -- WHNF and NF 
-- It's in normal form because while (*) has been applied to two arguments of a
-- sort it cannot be reduced fruther until the outer \x -> ...
-- has been applied. With nothing further to reduce it is in normal form
"aa" ++ "aa" -- neither WHNF nor NF

```

When the `(:)` is applied we have to realize one thing and that is the fact
that data is immutable in Haskelll when we map, we do not mutate the existing
list but build a new list with the values that result from applying the
function.

Strictness does not proceed only outside-in. We can have lazily evaluated code
wrapped around a strict core. In fact, we can choose to apply laziness and
strictness in how we evaluate the spinr or the leaves in dependently. A common
mantra for performance sensitive code in Haskell is "lazy in the spine, strict
in the leaves". 

# Definitions

## Product Types

In type theory, a _Product type_ is a type made of a set of types compounded
over each other. In Haskell, we represent products using tuples or data
constructors with more than one argument. The 
compounding_ is from each type argument to the data constructor representing a
value that coexists with all the other values simultaneously. Products of types
represent a conjunction i.e. `and` of those types. If you have a product of
Bool and Int, your terms will each contain a Bool and Int value.

## Sum Types

In type theory, a _Sum type_ of two types is a type whose terms are terms in
either type, but not simultaneously. In Haskell, sum types are represented usig
the pipe, `|`, ina data type definition. Sum of types represents a disjunction
i.e. `or` of those types. If you have a sum of Bool and Int, your terms will be
_either_ a Bool value or an Int value.

## Cons cell 

This is a data constructor and a product of the types `a` and `[a]` as defined
in the list datatype. Because it references the list type constructor itself in
the second argument, it allows for nesting of multiple con cells, possibly
indefinitely with the use of recursive functions, for representing an
indefinite number of values in series

## The `Spine`

This is a way to refer to the structure that glues a collection of values
together. In the list datatype, it is formed by the recursive nesting of cons
cells. The spine is, in essence, the structure of collection that isn't thje
values contained therein. Often spine will be used in reference to lists, but
it applies with tree data structures as well.

