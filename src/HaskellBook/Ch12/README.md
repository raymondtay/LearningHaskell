# Signaling adversity

Sometimes it is not convenient or possible for every value in a datatype to
make sense fo ryour programs. Whent hat ahppens in Haskell, we use explicit
datatypes to signal when our functions received a combination of inputs that
don't make sense.

The convention of using `Left` to signal a kind of error or failure is standard
in Haskell and that convention came about for a reason. Ther eason has to do
with the orderig of type arguments and application of functions. Normally it is
your error or invalid result that is going to cause a stop to whatever work is
being done by the program. You cannot apply functions to the left type
argument, and `Functor` won't map over it. Since you normally want to apply
functions and map over the case that doesn not stop your program, it has become
convention that the `Left` or `Either` is used for whatever case is going to
cause the work to stop.

# Kinds, a thousand stars in your types

Kinds are types one level up. They are used to describe the types of type
constructors. One noteworthy feature of 
haskell is that it has higher-lkinded types. Here the term kingher-kinded
derives from higher-order functions, functions that take more functions as
arguments. Type constructors (that is, higher-kinded types) are types tkae mroe
than types as arguments. The haskell report uses the term type constant to
refer to types that take note arguments and are already types. In the report,
type constructor is used to refer to types which must have arguments applied to
become a type.

## Lifted and unlifted types

To be precise, kind * is the kind of all standard lifted types, while types
that have the kind # are unlifted. A lifted type, which includes any datatypes
you caould define yourself, is any that can be inhabited by bottom. Lifted
types are represented by a pointer and include most of the data types we have
seen and most that you are likely toe encounter and use. Unlifted types are any
types which cannot be inhabited by bottom. Types of kind $ are often native
machine types and raw pointers. Newtypes are a special case in that they are
kind *, but are unlifted because their representation is identitical to that of
the type they contain, so the newtype itself is not creating any new pointer
beyond that of the type it contains. That fact means that the newtype itself
cannot be inhabited by bottom, only the thing it contains can be, so newtypes
are unlifted. The default kind of concrete, fully-applied datatypes in GHC is
kind *.

```haskell

*Chapter11> :k Maybe
Maybe :: * -> *

*Chapter11> :k (->)
(->) :: * -> * -> *

*Chapter11> :k (->) (Maybe Integer)
(->) (Maybe Integer) :: * -> *

*Chapter11> :k (->) (Maybe Integer) Integer
(->) (Maybe Integer) Integer :: *

*Chapter11> :k (->) (Maybe Integer) (Maybe Integer)
(->) (Maybe Integer) (Maybe Integer) :: *

```

Data constructors that have not been fully applied actually have a function
arrow in them ⇒ data constructors are really functions ! for example, see the
following 
```haskell
#> data X a = X a deriving Show

#> fmap X [4] -- lift X (since it's a function) over the list-of-4
#> [X 4]  -- list of X's
```

# Unfolds

While the idea of catamorphisms is still relatively fresh in our minds, let us
turn out attention to their dual: anamorphisms. If folds, or catamorphisms, let
us break data structures down then unfolds let us build them up. There are just
as with folds, a few different ways to unfold a data structure. We can use them
to create finite and infinite data structures alike.

An example of would be `unfoldr` from `Data.List`:

```haskell

import Data.List

let f = (\x -> x+1)
unfoldr (\x -> Just(f x, x)) 4 -- this goes into infinite loop

take 4 $ unfoldr (\x -> Just(f x, x)) 4 -- returns [5,5,5,5]
take 4 $ unfoldr (\x -> Just(x, f x)) 4 -- returns [4,5,6,7]

take 10 $ unfoldr (\x -> Just(x, x+1)) 0 - returns first 10 of the natural numbers

```
