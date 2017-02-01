# Typeclasses

Typeclasses and types in Haskell are, in a sense, opposites. Where a declaration of a type defines how that type in particular is created, a declaration of a typeclass defines how a set of types are consumed or used in computations.

This tension is realted to the expresison problem which is about definiing code in terms of how data is created or processed. As Philip Wadler puts it, "The goal is to define a datatype by cases, where one can add new cases to the dattype and new functions over the datatype, w/o recompiling existing code and while retaining static type safety (e.g. no casts)". 

The typeclass constraint is superfluous when the types are concrete. On the other hand, you must specify which typeclasses you want type variables to have implemented. The use of polymorphic values w/o the ability to infer a specific type and no default rule will cause GHC to complain about an ambiguous type.

What sets Haskell apart from most other functional programming languages is that it introduced and refined a means of writing ordinary programs that talk to the outside workd w/o adding anything to the pure lambda calculus it is founded on. This property - being lambda calculs and nothing more - is what makes Haskell a purely functional programming language.

## Typeclass deriving

Typeclass instances we can magically derive are `Eq`, `Ord`, `Enum`, `Bounded`, `Read` and `Show`, though there are some constraints on deriving some of these. Deriving means you don't have to manually write instance sof these typeclasses for each new dataype you create. 

### Read 

The `Read` typeclass is a lot like `Show`. This typeclass iss essentially the
opposite of `Show`. Where `Show` takes things and turns them into
human-readable strings, `Read` takes strings and turns them into things. Like
`Show`, its not a serialization format.

## Instances are dispatched by type

Typeclasses are dispatched by type, but it's an important thing to understand.
Typeclasses are defined by the set of operations and values all instances will
provide. The typeclass instances are unique pairings of the tytpeclass an
instance is being defined for and the type it's for.


Haskell does not provide universal stringification (Show/print) or equality or
pointer equality as this is not always sounds or safe, regardless of what
programming language you are using.


