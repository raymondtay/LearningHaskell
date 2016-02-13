# What makes these data types algebraic ?

Algebraic data types in HAskell are algebraic because we can describe the
patterns of argument structures using two basic operations: sum and product.
The most direct way to explain why they're called sum and product is to demonstrate
sum and product in terms of cardinality. This can be understood in terms of the
cardinality you see with finite sets. 

This doesn't map perfectly as we can have infinite data structures in Haskell, 
but it's a good way to begin understanding and appreciating how data types work. When
it comes to programming languages we are concerned with computable functions, 
but not just those which can generate a set.

# Higher-kinded data types

You may recall we discussed kinds earlier in this chapter. Kinds are the
types of type cosntructors, primarily encoding the number of arguments
they take. The default kind in Haskell is *. Kind signatures work like 
type signatures, suign the same :: and -> syntax but there are only a few
kinds and you'll most often see *.

Kinds are not types untill they are fully applied. Only types have inhabitants
at the term level. The kind * -> * is waiting for a single * before it is fully
applied. The kind * -> * -> * must be applied twice before it will be a real type.
This is known as a higher-kinded type. Lists, for example, are higher-kinded 
data types in Haskell.

Because types can be generically polymorphic by taking type arguments, they
can be applied at the type level.


## Infix type and data constructors

When we give an operator a non-alphanumeric name, it is infix by default. 
For example, all non-alphanumeric arithmetic functions are infix operators, 
while we have some alphanumeric arithmetic functions, such as div and mod 
that are prefix by default. So far, we have only seen alphanumeric 
data constructors, except for this constructor in the list type, but the same
rule applies to them.

Any operator that starts with a colon (:) must be an infix type or 
data constructor. All infix data constructors must start with a colon. The
type constructor of functions, (->), is the only infix type constructor
that doesn't start with a colon. Another exception is that they cannot be ::
as this syntax is reserved for type assertions.

