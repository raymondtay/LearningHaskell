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

