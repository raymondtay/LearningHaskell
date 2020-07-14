{-
  This is just regular stuff.
-}

data Foo a = Foo a

instance Functor Foo where
  fmap f (Foo a) = Foo (f a)

{-
  Adding a type constraint
  -------------------------
  When we define a new type , we can add a type constraint just after the `data` keyword
  which means that we can place a type `a` to `Bar` iff `a` is a member of the typeclass `Eq`.
  On page 247 of the book, it reads 
  '''
  Adding a constraint to a type definition is essentially never a good idae. It has the effect
  of forcing you to add type constraints to every function that will operate on values of that type.
  Let's say that we need a stack data structure that we want to be able to query 
  to see whether its elements obey some ordering and here's a naive version of that
  '''
-}

data Eq a => Bar a = Bar a

instance Functor Bar where
  fmap f (Bar a) = Bar (f a)

data (Ord a) => OrdStack a = Bottom | Item a (OrdStack a) deriving (Show)

