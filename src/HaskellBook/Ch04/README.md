# Datatypes in Haskell

Types are a powerful means of classifying, organizing and delimiting to only the forms we want to process in our programs. Types provide the means to build programs more quickly and also allow for greater ease of maintainence.

Data declarations are how datatypes are defined.

The type constructor is the name of the type and is capitalized When you are reading or writing type signatures (the type level of your code), the type names or type constructors are what you use. 

Data constructors are the values that inhabit the type they are defined in. They are the values that show up in your code, at the term level instead of the type level. By "term level", we mean they are the values as the appear in your code or the values that your code evaluates to.

```haskell

data Bool = True | False

```

- Type constructor for the datatype `Bool`. This is the name of the type and shows up in type signatures.
- Data constructor for the value `False`.
- Pipe `|` indicates logical disjunction, `or`. So a `Bool` value is `True` or `False`.
- Data constructor for the value `True`.

The whole thing is called a data declaration. Data declartions do not alwways follow precisely the same pattern - there are datatypes that use logical conjunction ("and") instead of disjunction, and some type constructors and data constructors may have arguments. The thing they have in common is the 
keyword data followed by the type constructor (or name of the type that will appear in type signatures), the equals sign to denote
a definition, and then data constructors (or names of values that inhabit your term-leve code.

These numeric datatypes all have instances of a typeclass called `Num`. Typeclasses are a way of adding functionality to types that is resuable across all the types that have instances of that typeclass. `Num` is a typeclass for which most numeric types will have an instance because there are standard functions that are convenient to have available for all types of numbers. The `Num` typeclass is what 
provides your standard`(+)`, `(-)` and `(*)` operators along with a few others. Any type that has an instance of `Num` can be used with those functions.
