# Functional Patterns

## Pattern Matching 

It is worth noting there that _patteerns_ can include things as diverse as
undefined variables, numeric literals, and list syntax;it also extends to any
and all data constructors.

Patterns are matched against values, or data constructors, not types. Matching
a pattern may fail, proceeding to the next available pattern to match or
succeed. When a match succeeds, the variables exposed in the pattern are bound.
Pattern matching proceeds from left to right and outside to inside.

## Higher-order functions

Higher-order functions are functions that accept functions as arguments.
Functions are just values - why couldn't they be passed around like any other
values? This is an important component of functional programming and gives us a
way to combine functions efficiently.

Here's an example of how a function is defined.
```
returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a
```

## Pointfree style

Pointfree refers to a style of composing functions without specifying their
arguments. The point in pointfree refers to the arguments, not to the function
composition opeartors. Quite often, the pointfree code is tidier on the page
and easier to read as it helps the reader focus on the functions rather than
the data that is being shuffled around.

