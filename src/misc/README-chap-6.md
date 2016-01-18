# Typeclass instances are dispatched by type

We've said a few times, w/o explaining it, that typeclasses are 
dispatched by type, but it's an important thing to understand. 
Typeclasses are defined by the set of operations and values all instances
will provide. 

The typeclass instances are unique pairings of the typeclass an instance is
being defined for and the type it's for.

A typeclass is the following:
+ a typeclass defines a set of functions and/or values
+ types have instances of that typeclass
+ the instances specify the ways that type uses the functions of the typeclass

