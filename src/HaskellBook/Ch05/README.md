# Types 

Haskell has a robust and expressive type system. Types play an
important role in the readability, safety, and maintainability of Haskell code.

# What are types?

Expressions when evaluated reduce to vales. Every value has a type.
Types are how we group a set of values together that share something in common.
In Haskell, you cannot create untyped data, so except for a sprinkling of 
syntactic sugar for things like numbers or functions, everything originates 
in a data constructor from some definition of a type.

Haskell's type system allows for a nifty feature known as type inference. 
We can declare our types when we write our programs, but the compiler will infer
the types for expressions that have no declared type. It is better
to have explicit type declarations in any nontrivial program but type inference 
can be helpful as i'm learning and experimenting with writing
new programs.

# Manual currying and uncurrying

Haskell is curried by default, but you can uncurry functions. "Uncurrying" means un-nesting the functions and replacing the two functions with a tuple of two values (these would be the two values you want to use as arguments). 

```haskell
let curry f a b = f (a,b)
:t curry
curry :: ((t1, t2) -> t) -> t1 -> t2 -> t

let uncurry f (a, b) = f a b
:t uncurry
uncurry (t1 -> t2 -> t) -> (t1, t2) -> t
```

*Note*: Turns out that you can check the types of things that aren't really implemented yet, so long as you give GHCi an `undefined` to bind the signature to.

# About parametric polymorphic functions

The arguments in parametric polymorphic functions, like `id`, could be anything, any type or typeclass, so the terms of the function are more restricted because there are no methods or information attached to them.

With the type `id :: a -> a ` it can do nothing other than return `a`
because there is no information or method attached to its parameter at all - nothing can be done with `a`.

If a type is a set of possible values, then a type variable represents a set of possible types. 
When there is no typeclass constraint, the set of possible of possible types a variable could represent is effectively unlimited.
Typeclass constraints limits the set of potential types (and thus, potential values) while also passing along the common functions that can be used with those values.

In sum, if a variable could be anything, then there's little that can be done to it because it has no methods. If it can be some types (say, a type that is an instance of Num) then it has some methods. If it is a concrete type, you lose the flexibility but, due to the additive nature of typeclass inheritance, gain more potential methods. It's important to note that this inheritance extends downward fro a superclass, such as `Num` to subclasses such as `Integral` and then `Int` but not the other way around.


## Asserting types for declarations

Most of the time, we want to declare our types, rather than relying on type inference. 
Adding type signatures to your code can provide guidance to you as you write your functions.
It can also help the compiler give you information about where 
your code is going wrong. As programs become longer and more complex, type signatures become even more important, as they help you or other programmers trying to use your code read it and figure out what it's supposed to do.

