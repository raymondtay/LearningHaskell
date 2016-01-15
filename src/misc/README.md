# Polymorphism

Polymorphic type variables give us the ability to implement
expressions that can accept arguments and return results of
different types without having to write variations on
the same expression of each type. It would be inefficient
if you were doing arithmetic and had to write the same code
over and over for different numeric types. The good news 
is the numerical functions that come with GHC installation
and Haskell Prelude are polymorphic by default. Broadly
speaking, type signatures may have three kinds of types:

+ Concrete
+ Constrained Polymorphic
+ Parameterically Polymorphic

In Haskell, polymorphism divides into two categories:
parameteric polymorphism and constrained polymorphism. If 
you have encountered polymorphism before, it was probably 
a form of constrained, often called ad-hoc, polymorphism.
Ad-hoc polymorphism in Haskell is implemented with typeclasses.

## Parametric Polymorphism

This is broader than ad-hoc polymorphism. Parameteric 
polymorphism refers to type variables, or parameters, that
are fully polymorphic. When unconstrained by a typeclass, their
final concrete type could be anything.

## Contrained Polymorphism

This OTOH puts typeclass constructors on the variable, 
decreasing the number of concrete types it could be, but
increasing what you can actually do with it by 
defining and bringing into scope a set of operations.

Recall that when you see a lowercase polymorphic function:
`identity`. The `id` function that comes with Haskell 
Prelude and is called the identity function because it is
the identity for any value in our language. 

# Type Inference

Haskell does not obligate us to assert a type for every
expression or value in our programs because it has 
type inference. Type inference is an algorithm for 
determining the type of expressions; built on top of 
Damas-Hindley-Milner type system.

Essentially, the compiler starts from the values whose
types it knows and then works out the types of the other
values. As you mature as a Haskell programmer, you would 
find this is principally useful for when you are still
figuring out new code rather than for code that is done.
Once your program is done, you will certainly know the types
of all the functions, and it's considered good practice 
to explicitly delcare them.


