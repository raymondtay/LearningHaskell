# Traversable

The interesting thing about `Traversable` is this, what it does is 
actually quite interesting. In order to understand what it is that `Traversable` does, it helps when i can 
actually observe what it does and from my observation, what it seems to do is the following

`traverse` actually maps each element of the structure to an action, evaluates
these actions from left-to-right and collecting the results. 

Below illustrates what happens...

```haskell

Prelude Data.Functor.Identity> :i traverse

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    ...
-- Defined in ‘Data.Traversable’
Prelude Data.Functor.Identity> traverse (Identity . (+1)) [1,2]
Identity [2,3]
Prelude Data.Functor.Identity> runIdentity $ traverse (Identity . (+1))
[1,2]
[2,3]

```

Let's visit some laws of the Traversables

# Traversable Laws

+ Naturality

  `t . traverse f = traverse (t . f)` 

  This laws tells us that function composition behaves in unsurprising ways
  with respect to a traversed function. Since a traversed function f is generating
  the structure that appears on the "outside" of the traverse operation, there's 
  no reason we shouldn't be able to float the function over the structure into the 
  traversal itself.

+ Identity

  `traverse Identity = Identity`

  This laws says that traversing the data constructor of the Identity type over a value
  will produce the same result as just putting the value in Identity. This tells us 
  Identity represents a "structural" identity for traversing data.
  This is another way of saying that a Traversable instance cannot add or
  inject any structure or "effects".

+ Composition

  `traverse (Compose . fmap g . f) =
    Compose . fmap (traverse g) . traverse f`

  This law demonstrates how we can collapse sequential traversals into a single
  traversal, by taking advantage of the Compose datatype, which combines
  structure.


