# Folds

Folding is a concept that extends in usefulness and importance beyond lists, 
but lists are often how they are introduced. Folds as a general concept
are called catamorphism.

## Fold right

We call `foldr` the right fold because the fold is right associative, that is
it associates to the right. This is syntactically reflected in a straightforward
definition of foldr as well:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
```

The similarities between this and the recursive patterns we saw above should be clear.
The "rest of the fold" (foldr f acc xs) is an argument to the function `f` we are 
folding with. The `acc` is the accumulator sometimes called "zero" of our fold. 
It provides a fallback value for the empty list case and a second arugment to begin 
our fold with. The accumulator is often the identity for whatever function we are folding
such as 0 for (+) for 1 for (*).

