# Traversable

Functor gives us a way to transform any values embedded in structure. 
Applicative, we saw, is a monoidal functor, and gives us a way to transform
any values contained within a structure using a function that is also
embedded in structure. This means that each application produces the effect of 
adding structure which is then applicatively combined. Foldable gives us a way
to process values embedded in a structure as if they existed in a sequential order.

Traversable depends on Applicative, and thus Functor and is also superclassed by Foldable.

Traversable allows you to transform elements inside the structure like a Functor, producing
Applicative effects along the way, and lift those potentially multiple instances of Applicative
structure outside of the Traversable structure. It is commonly described as a way to
traverse a data structure, mapping a function inside a structure while accumulating the
applicative contexts along the way. This is easiest to see, perhaps, through liberal 
demonstration of examples, so let's get to it.

For starters, let's take a look at the original
definition:
```haskell
class (Functor t, Foldable t) => Traversable t where

  -- Map each element of a structure to an action,
  -- evaluate these actions from left to right, and 
  -- collect the results. For a version that ignores
  -- the results see 'Data.Foldable.traverse_'
  traverse :: Applicative f =>
       (a -> f b)
    -> t a
    -> f (t b)
  traverse f = sequenceA . fmap f

  -- Evaluate each action in the structure from left to
  -- right, and collect the results. 
  -- For a version that ignores the results see
  -- 'Data.Foldable.sequenceA_'
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

```
Let's try to understand `sequenceA` which looks like it 
switches the two contexts of the structure. It doesn't by
itself allow you to apply any function to the 'a' value inside
the structure: it only flips the layers of structure around

Comparing the following 
```haskell
$> sum [1,2,3]
6
$> fmap sum [Just 1, Just 2, Just 3] -- equivalently: fmap sum $ fmap Just [1,2,3]
[1,2,3]
$> (fmap . fmap) sum Just [1,2,3]
Just 6
$> fmap product [Just 1, Just 2, Nothing]
[1,2,1]

``` 
to these ...
```haskell
$> fmap Just [1,2,3]
[Just 1, Just 2, Just 3]

$> sequenceA $ fmap Just [1,2,3]
Just [1,2,3]

$> sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]

$> sequenceA [Just 1, Just 3, Nothing]
Nothing

$> fmap sum $ sequenceA [Just 1, Just 2, Just 3]
Just 6

$> fmap product $ sequenceA [Just 1, Just 2, Just 3]
Just 6

$> fmap product $ sequenceA [Nothing, Just 2, Just 3]
Nothing
```

##  Understanding traverse and mapM

Based on the typeclass definition we saw earlier, we know that
`traverse` is basically `fmap` composed with `sequenceA` and 
that is :
`traverse f = sequenceA . fmap f`

and it is helpful to understand how things work by examining examples:
```haskell
$> fmap Just [1,2,3]
[Just 1, Just 2, Just 3]
$> sequenceA $ fmap Just [1,2,3]
Just [1,2,3]
$> sequenceA . fmap Just [1,2,3]
Just [1,2,3]
$> traverse Just [1,2,3]
Just [1,2,3]
```

with `traverse`, you won't have to change your code
because the primary Vector datatype has a Traversable instance
and so should work fine.

Similarly, the type for `sequence` in GHC versions prior to 7.10
is just a less useful `sequenceA`.
```haskell
sequence :: Monad m =>
  [ m a ]
->  m [a]

-- contrast the above with this 
sequenceA :: (Applicative f, Traversable t) =>
    t (f a)
->  f (t a)

```
Again, we are generalizing the list to any Traversable and weakening the Monad
requirement to just Applicative.

# So what is traversable for?

In a literal sense, anytime you need to flip two type constructors
around, or map something and then flip them around, that's probably
Traversable.
```haskell
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
sequenceA :: Applicative f => t (f a) -> f (t a)
```

Before, i said that `traverse` is like `sequenceA` and here's how they are alike
```haskell
$> let f = undefined :: a -> Maybe b
$> let xs = undefined :: [a]
$> :t map f xs 
map f xs :: [Maybe b]
$> :t sequenceA $ map f xs
sequenceA $ map f xs :: Maybe [a]
$> :t traverse f xs
traverse f xs :: Maybe [b]
```


