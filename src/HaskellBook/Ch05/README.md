# Types 

Haskell has a robust and expressive type system. Types play an
important role in the readability, safety, and maintainbility of Haskell code.

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

