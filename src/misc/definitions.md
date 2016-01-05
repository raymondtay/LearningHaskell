# Definitions

+ A `tuple`
```
A tuple is an ordered grouping of values. In haskell, there is no 
singleton tuple, but there is a zero tuple also called unit or (). 
The types of elements of tuples are allowed to vary, so you can have
both (String, String) or (Integer, String). Tuples in Haskell are
the usual means of expressing an anonymoud product.
```

+ A `typeclass`
```
A typeclass is a set of operations defined with respect to a polymorphic
type. When a type is instance of a typeclass, values of that type can be 
used in the standard operations defined for that typeclass. In 
Haskell type classes are unique pairings of class and concrete instance.
This means that if a given type a has an instance of Eq, it has only
one instance of Eq.
```

+ A `data constructor`
```
Data constructors in Haskell provide a means of creating values that
inhabit a given type. Data constructors in haskell have a type and
can either be constant values (nullary) or take one or more arguments
just like functions. In the following example, Cat is a nullary data
constructor for Pet and Dog is a data constructor that takes an argument.
```
```haskell
type Name = String
data Pet = Cat | Dog Name
```

+ A `type constructor`
```
Type constructors in Haskell are not values and can only be used in 
type signatures. Just as data declarations generate data constructors to 
create values that inhabit that type, data declarations generation
type constructors which can be used to denote that tyep. In the above
example, Pet is the type constructor. A guideline for differentiating 
the two kinds of constructors is that type constructors always go to 
the left of the = job in a data declaration.
```

