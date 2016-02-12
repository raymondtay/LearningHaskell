# Folds

Folding is a concept that extends in usefulness and importance beyond lists, 
but lists are often how they are introduced. Folds as a general concept
are called catamorphism.

## About Folds

A fold is a higher order function which, given a function to accumulate
the results and a recursive data structure, returns the built up value.
Usually, a start value for the accumulation is provided along 
with a function that can combine the type of values in the data
structure with the accumulation. The term fold is typically usd with 
reference to collections of values references by a recursive datatype.
For a generalization of breaking down structure, see catamorphism.

## About Catamorphism

A catamorphism is a generalization of folds to arbitrary datatypes where a fold
allows you to break down a list into an arbitrary dataytpe, a catamorphism 
is a means of breaking down the structure of any dataytype. The bool :: a -> a -> Bool -> a
function in Data.Bool is an example of a simple catamorphism for a simple non-colection
data type. Similarly, maybe :: b -> (a -> b) -> Maybe a -> b is 
the catamorphism for Maybe. 

```haskell

data Bool = True | False
bool :: a -> a -> Bool -> a

data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b

data Either a b = Left a | Right b
either :: (a -> c) -> (b -> c) -> Either a b  -> c

```


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

