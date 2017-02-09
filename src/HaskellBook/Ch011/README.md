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


