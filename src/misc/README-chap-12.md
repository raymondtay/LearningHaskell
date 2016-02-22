
# Kinds, a thousand stars in your types

Kinds are types one level up. They are used to describe the types of type
constructors. One noteworthy feature of Haskell is that it has higher-kinded 
types. Here the term higher-kinded derives from higher-order functions, funcitons
that take more functions as arguments. Type constructors (that is, higher-kinded types)
are types that take more types as arguments. The Haskell report uses the term type
cosntant to refer to types that take no arguments and are already types. In the 
Report, type constructor is used to refer to types which must have arguments applied
to become a type.


# Data constructors are functions

We note the difference between data cosntants and constructors
and noted that data constructors that haven't been fully applied have a 
function arrow in them. Once you apply them to their argument,
they returna  value of the appropriate type. In other words, data
constructors really are functions. Hence, they behave like Haskell
functions in that they are curried as well. 

