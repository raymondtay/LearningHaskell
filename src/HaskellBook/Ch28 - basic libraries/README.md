
## Constant Applicative Forms

When we are talking about memory usage and sharing in Haskell, we have to also
talk about CAFs: constant applicative forms. CAFs are expressions that have no
free variables and are held in memory to be shared with all other expressions
in a module. They can be literal values or partially-applied functions, as long
as the arguments to the function are not named arguments.

