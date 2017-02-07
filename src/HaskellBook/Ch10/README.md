# Folds 

Folding is a concept that extends in usefulness and importance beyond lists,
but lists are often how they are introduced. Folds as a general concept are
called `catamorphisms`. Catamorphisms are a means of deconstructing data. If
the spine of a list is the structure of a list, then a fold kis what can reduce
that structure.

Take note that a catamorphism can break down the structure but that structure
might be rebuilt, so to speak, during evaluation. That is, folds can return
lists as results.

# Fold right

`foldr` is called the "right fold" because the fold is right associative;which
means it associates to the right. 
```haskell
foldr :: (a -> b ->b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
```

A non-obvious aspect of folding is that it happens in two stages, traversal and
folding. Traversal is the stage in which the fold recurses over the spine.
Folding refers to the evaluation or reduction of the folding function applied
to the values. All folds recurse over the spine in the same direction; the
difference between left folds and right folds is in the association, or
parenthesization, of the folding function and, thus, whic direction the folding
or reduction proceeds.

# Fold left

`foldl` unconditionally evaluates the spine and this generally means that
`foldl` is inappropriate with lists that are or could be infinite, but the
combination of the forced spine evaluation with non-strictness means that it is
also usually inappropriate even for long lists, as the forced evaluation of the
spine affects performance negatively. Because `foldl` must evluate its whole
spine before it starts evaluating values in each cell, it accumulates a pile of
unevaluated values as it traverses the spine.

In most cases,when you need a left fold, you should use `foldl`. This function,
called `fold-l-prime` works the same except that it is trcit. In other words,
it forces evlaution of the values inside cons cells as it traverses the spine,
rather than accumulating unevaluated expressions for each element of the list.
The strict evaluation here means it has less negative effect on performance
over long lists.

## How to write fold functions

When we write folds, we being by thinking about what our start value for the
fold is. This is usually the `identity` for the function. So when we sum the
elements of a list, the identity of summation is 0. when we multuple the
elements of the list, the identity is 1. This start value is also our fallback
in case the list is empty.

# Scans

Scanas work similarly to maps and also to folds. Like folds, they accumulate
values instead of keeping the list's individual values separate. Like maps,
they return a list of results. In this case, the list of results shows the
intermediate stages of evaluation, that is, the values that accumulate as the
function is doing its work.

# Definitions

A `Fold` is a higher-order function which given a function to accumualte the
results and a recursive data structure, returns the built up value. Usually a
start value for the accumulation is provided along with a function that can
combine the type of values in the data structure with the accumulation. The
term fold is typically used with reference to collections of values referenced
by a recursive data type.

A catamorphism is a generalization of folds to arbitrary datatypes. Where a
fold allows you to nbreak down a list into an arbitrary datatype, a
catamorphism is a means of breaking down the structure of any datatype. The
`bool :: a -> a -> Bool -> a` function in `Data.Bool` is an example of a simple
catamorphism for a simple non-collection datatype. Similarly, `maybe :: b -> (a
-> b) -> Maybe a -> b` is the catamorphism for `Maybe`.

A `tail call` is the final result of a function. Some examples of tail calls in
Haskell functions:
```haskell
f x y z = h (subFunction x y z) 
  where subFunction x y z = g z y z
```

Tail recursion is a function whose tail calls are recursive invocations of
itself. This is distinguished from functions that call other functions in their
tail call.


