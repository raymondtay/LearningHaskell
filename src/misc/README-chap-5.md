# Definitions

+ Polymorphism this refers to type variables which may refer to more than 
  one concrete type. In Haskell, this will usually manifest as parametric
  or ad-hoc polymorphism. By having a larger set of types, we intersect 
  the commonalities of them all to produce a smaller set of correct terms.
  This makes it less likely we will write an incorrect program and lets
  us reuse the code with other types.

+ With respect to Haskell, the principal type is the most generic type
  which still typechecks. More generally, principal type is a property of
  the type system you're interacting with. Principal typing holds for
  that type system if a type can be found for a term is an environment
  for which all other types for that term are instances of the principal
  type. Here are some examples:
  ```haskell
  a
  Num a => a
  Int
  -- the principal type here is the 
  -- parametrically polymorphic 'a'

  (Ord a, Num a) => a
  Integer
  -- The principal type is (Ord a, Num a) => a
  ```

+ Type variable is a way to refer to an unspecified type or set of types
  in Haskell type signatures. Type variables ordinarily will be equal to 
  themselves throughout a type signature. Let's consider some examples:
  ```haskell
  id :: a -> a
  -- one type variable 'a' that occurs twice, 
  -- once as an argument, once as a result.
  -- parameterically polymorphic, could be strictly
  -- anything.
  (+) :: Num a => a -> a -> a
  -- One type variable 'a' , constrained to needing
  -- an instance of Num. Two arguments, one result
  -- all the same type.
  ```

+ Ad-hoc polymorphpism (sometimes called "constrained polymorphism")
  is polymorphism that applies one or more typeclass constructors
  to what would have otherwise been a parametrically polymorphic 
  variable. Here, rather than representing a uniformity of behavior
  across all concrete applications, the purpose of ad-hoc polymorphism 
  is to allow the functions to have different behavior for each instance.
  This ad-hoc mess is constrained by the types in the typeclass that defines 
  the methods and Haskell's requirement that typeclass be unique 
  for a given type. For any given combination of typeclass and
  a type, such as `Ord` and `Bool`, there must only exist one unique 
  instance in scope. This makes it considerably easier to reason about 
  typeclasses.

