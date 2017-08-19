
# Functional Dependencies

Functional dependencies are used to constrain the parameters of type classes.
They let you state that in a multi-parameter type class, one of the parameters
can be determined from others, so that the parameter determined by others can,
for example, be the return type but none of the arguments types of some of the
methods.


You can express this by specifying a functional dependency, or fundep for
short:

```haskell
class Mult a b c | a b -> c where
  (*) :: a -> b -> c
  
```
This tells Haskell that 'c' is uniquely determined from 'a' and 'b'.

