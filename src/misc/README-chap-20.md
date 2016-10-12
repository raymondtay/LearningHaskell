# Foldable 

The folding function is always dependent on some `Monoid` instance. The folds
we wrote previously mostly relied on implicit monoidal operations. Generalizing 
catamorphisms to other datatypes depends on understanding the monoids, for 
those structures and in some cases making them explicit.

```haskell

data Identity a = Identity a 

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

```
You can define this over the `ghci` console:
```haskell
* Data.Foldable> :{
* Data.Foldable| data Identity a = Identity a
* Data.Foldable| instance Foldable Identity where
* Data.Foldable|   foldl f z (Identity x) = f z x
* Data.Foldable|   foldr f z (Identity x) = f x z
* Data.Foldable|   foldMap f (Identity x) = f x
* Data.Foldable| :}
```

