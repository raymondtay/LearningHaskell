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
such as 0 for (+) for 1 for (\*).

Here's a nice trick to impress your friends with:

```haskell
foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" $ map show [1...5]
```
produces the following 
```
"(1+(2+(3+(4+(5+0)))))"
```


One initially non-obvious aspect of folding is that it happens in two stages
traversal and folding. Traversal is the stage in which the fold recurses 
over the spine. Folding refers to the evaluation or reduction of the folding 
function applied to the values. All folds recurse over the spine in the same 
direction: the difference between left folds and right folds is in the association
or parenthesization of the folding function and thus which direction the folding 
or reduction proceeds.

```haskell
foldr (+) 0 (take 5([1,2,3,4,5] ++ undefined))
```

