# Definitions (more ...)

+ `Data declarations`
```
Data declarations define new data types in Haskell. Data declarations 
always create a new type constructor, but may or may not create
new data constructors. Data declarations are how we refer to the entire
definition that begins with the data keyword.
```

+ `Type aliases`
```
A type alias is a way to refer to a type constructor or type constant by
an alternate name, usually to communicate something more specific
or for brevity.
```
```haskell
type Name = String
-- creates a new type alias Name of the 
-- type String *not* a data declaration.
--- just a type alias declaration
```
+ `Arity`
```
Arity is the number of arguments a funciton accepts. This notion is a little
slippery in Haskell, as due to currying, all functions are 1-arity
and we handle accepting multiple arguments by nesting functions.
```
+ `Polymorphism`
```haskell
Polymorphism in haskell means being able to write code in terms of 
values which may be one of several, or any, type. Polymorphism in
haskell is either parametric or constrained. The identity function, id,
is an example of a parametrically polymorphic function:

id :: a -> a
id x = x

Here id works for a value of any type because it doesn't use any information 
specific to a given type or set of types. Whereas, the following function
isEqual:

isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y

IS polymorphic, but constrained or bounded to the set of types which
have an instance of Eq typeclass.
```





























