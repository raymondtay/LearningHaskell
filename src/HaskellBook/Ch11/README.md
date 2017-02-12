# Algebraic data types

The purpose of this chapter is to explain how to construct your 
own datatypes in Haskell. Writing your own datatypes can help you leverage
some of haskell's most powerful features - pattertn matching, type checking and
inference - in a way that makes your code more concise and safer. But to
understand that, first we need to explain the differences among datatypes more
fully and understand what it means when we say datatypes are algebraic.

A type can be thought of as an enumration of constructors that have sero or
more arguments. Haskell offers sum types, product types, product types with
record syntax, type aliases and a special datatype called a _newtype_ that
offers a different set of options and constraints from either type synonyms or
data declarations.

## The approach

We often want to create cusom datatypes for structuring and describgint ehd ata
we are processing. Doing so can hlpe you anaze your problem by allowing you to
focus first on how you model the doamin before you begin thinking about how you
write computations that solve your problem. It can also make your code easier
to read and use because it lays the domain model out clearly. In order to writ
e your own types, though, you must understand the way datatypesa re constructed
in more detail than we have covered so far. Let's begin with a review of
the important parts of datatypes, using the data constructors for `Bool` and
`lists`:

# Data and Type constructors

There are two kinds of constructors in Haskell: type constructors and data
constructors. Type constructors are used only at the type level, in type
signatures and typeclass declarations and instances. Types are static and
resolve at compile time. Data constructors construct the values at term level,
values you can interact with at runtime. 

Type and data constrcutros that take no arguments are constatns. They can only
store a fixed type and amount of data. However, sometimes we need the
flexibility of allowing diferent types or amounts of data to be stored in our
datatypes. For those times, type and data constructors may be parameteric. When
a constructor takes an argument, then it is like a function in at least one
sense - it must be applied to become a concrete type or value. The following
datatypes are psedonymous versions of real datatypes in Haskell.


# What's a type and what's data?

Types are static and resolve at compile time. Types are known before runtime,
whether through explicit declaration or type inference, and that's what makes
them static types. Information about types does not persist through to runtime.
Data are waht we are working with at runtime.

Here compile time is literally when your program is getting compiled by GHC or
checked before execution in a REPL like GHCi. Runtime is the actual execution
of your program. Types circumscribe values and in that way, they describe which
values are flowing through what parts of your program.

# What makes these datatypes algebraic

ADTs in Haskell are algebraic because we can descirbe the patterns of argument
structures using two basic operations: sum and product. The most direct way to
explain why they are called sum and product is to demonstrate sum and prodcut
in terms of cardinality. This can be understood in terms of the cardinality you
see with finite sets. This doesn't map perfectly as we can have infinite data
structures in Haskell, but it's a good way to begin understanding and
appreciating how datatypes work.

# Sum Types

The `|` represents logical disjunction - that is , "or". This is the _sum_ in
algebraic datatypes. To know the cardinality of sum types, we _add_ the
cardinalities of their data constructors. `True` and `False` take no type
arguments and thus are nullary constructors, each with a value of 1.

# Product Types

What does it mean for a type to be a product? A product type's cardinality is
the _product_ of the cardinality of its inhabitants. Arithmeticallh speaking,
products are _multiplication_. Where a sum type expresses `or`, product types
talk about `and`.

# Higher-kinded datatypes

Kinds are the types of type constructors, primarily encoding the number of
arguments they take. The default kind in Haskell is *. Kind signatures work
like type signatures, using the same :: and -> syntax, but there are only a few
kinds and you will most often see *.

Kinds are not types until they are fully applied. Only types have inhabitants
at the term level. The kind * -> * is waiting for a single * before it is fully
applied. The kind * -> * -> * must be applied twice before it willbe a real
type. This is knmown as a higher-kinded type.

Gettig comfortable with higher-kinded types is important as type arguments
provide a genric way to express a hole to be filled by consumers of your
datatype later. 

## Lists as polymorphic types

Any operator that starts with a colon must be an infix type or data
constructor. All infix data constructors must start with a colon. The type
constructor of functions (->) is the only infix type constructor that doesn't
start with a colon. Another exception is that they cannot be :: as this syntax
is preserved for type assertions.

Type constructors are functions one level up, structuring things that cannot
exist at runtime - it is purely static and describes the srtcuture of our
types.

