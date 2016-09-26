
# Definitions

In type theory, a Product type is a type made of a set of types
compounded over each other. In haskell, we represent products either
using tuples or data constructors with more than one argument. The 
compounding is from each type argument to the data constructor representing
a value that coexists with all the other values simultaneously. 
Products of types represent a conjnction, "and", "of those types. 
If you have a product of Bool and Int, your terms will each contain a
Bool and Int vlaue.

In type theory, a Sum type of two types is a type whose terms are
terms in either type, but not simultaneously. In Haskell, sum types
are represented using the pipe, |, in a data type definition. 
Sums of types represent a disjunction, "or", of those types. In you have a 
sum of Bool and Int, your terms will be either a Bool value or an Int value.

Cons is ordinarily used as a verb to signify that a list value has been
created by consing a value onto the head of another list value. In Haskell,
(:) is the cons operator for the list type. It is a data constructor defined
in the list datatype

One thing to note is that we use _ (i.e. _underscore_) to ignore the values in our arguments or that are part of a pattern match. In this case, we pattern-matched on the (:) data constructor, but wanted to ignore the value which is the first argument. However, it is not a mere convention to bind references we don't care about on the left hand side to _. You cannot bind arguments to the name "_"; it's part of the language. This is partly so the compiler knows for a certainty you would not ever evaluate something in that particular case. Currently if you try it, it would think you're trying to refer to a hold if you use _ on the right-hand side in the definition.

