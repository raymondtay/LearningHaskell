# To Fix or Not To Fix, that is the question

## Let's start …
If you have programmed Scala in the functional programming style or Haskell
(there's only but functional programming here), then chances are that you have
heard of recursion schemes. These are fascinating structures in the FP paradigm
which gives you the ability to reduce boiler plate code in your projects. The most
iconic paper written on the subject was published in 1991 and i recommend you to
read it, if you have not.
When i first learnt about it, i did not appreciate what the fuss was all about
and its application is not immediately obvious to me. It took me like a while
to "get it". Once i saw it, there is no turning back and this is where you
need to give more thought to writing readable code and make sure that its readable
to other people in your team, besides yourself, because it can get cryptic very
quickly. Having said that, it has its uses.

## ---

When i first learnt about the notion of recursion schemes, it came from the Scala
world where people were talking about it and i started grokking about the subject
(pouring through papers, blog posts & examining code snippets written by various
authors etc) and if you are using the Cats libraries from Typelevel, then i would
recommend you give Matryoshka a try.

I approach learning programming languages like how i approach math, i take a small
example and work through it, incrementally, and build up knowledge.
The code examples here are therefore written in this same spirit.
I start with a nested and recursive definition of a `List` and a `Tree`.
The canonical representation is as follows:

```haskell
data List' a = Nil' | Cons' a (List' a ) deriving Show
data Tree  a = Leaf a | Node a (Tree a) (Tree a) deriving Show
```

and one of the most common patterns of computing such data structures is traversing
such structures with the purpose of applying some function to it, its call a `map`
and in Haskell `map (+1) [1..4]` would give `[2,3,4,5]` . In the above expressions,
you can write such a representation too! This rationale applies to the Tree too,
in case you are wondering.

The key insight here is not the fact that the ADTs are defined in terms of themselves,
but rather the general structure has revealed which you can apply generality to them.
To do that, we need to introduce a new structure

```haskell
data Fix s a = FixT { getFix :: s a (Fix s a) }
-- the "s" represents the shape and "a" represents the instance of the type
```

What this expression is saying, is that s actually represents a kind-signature * → * → *
(that means s needs two other "things" to become a type and a represents * (that's
Haskell speak for instance or value). Next, i rewrite the previous definitions of `List` and `Tree` 

```haskell
data List_ a r = Nil_ | Cons_ a r deriving (Show)
data Tree_ a r = Leaf_ a | Node_ a r r deriving (Show)
type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a
```

It's instructive to think of the r to mean "the part where it recurses" and you
can see this by comparing over the original List and Tree definitions. Now i have
2 type aliases that refers to this data type. Once we get past that, i want to be
able to write a function that performs map over this new data type, `Fix` that was
introduced. In the code snippets below, i have defined `mapL` and `mapT` to refer
to traversing over the new List and Tree data structures that are now defined in terms of Fix .

```haskell
mapL :: (t -> a) -> Fix List_ t -> Fix List_ a
mapL f listF =
  case list_ of
    (Cons_ x r) -> FixT (Cons_ (f x) (mapL f r))
    Nil_        -> FixT Nil_
    where list_ = getFix listF
mapT :: (t -> a) -> Fix Tree_ t -> Fix Tree_ a
mapT f treeF =
  case tree_ of
    (Node_ x l r) -> FixT (Node_ (f x) (mapT f l) (mapT f r))
    (Leaf_ x)     -> FixT (Leaf_ (f x))
    where tree_ = getFix treeF
```

What these functions essentially do, is to peel away the skins of the nested-recursive
data values and apply a function to the layer that has been peeled away. It's akin
to peeling a banana, an onion. The key insight here is that neither `mapT` or `mapL`
alters the form or shape of the data values and that is a good thing because it obeyed
the semantics of map which is aka structure preserving transformation.

If you try running this code in the Haskell REPL, you'll quickly discover that you are
not able to print the structure out. In Haskell, the typical solution is write an
instance of the type-class Show for `FixT List_ a` and `FixT Tree_ a`. In other words,
you need a function that understands how to stringify the `FixT` values.
```haskell
showListF :: (Show a) => ListF a -> String
showListF (FixT (Cons_ x r)) = (show x) ++ ", " ++ (showListF r)
showListF (FixT        Nil_) = "Nil_"

showTreeF :: (Show a) => TreeF a -> String
showTreeF (FixT (Node_ x l r)) = (show x) ++ ", " ++ (showTreeF l) ++ ", " ++ (showTreeF r)
showTreeF (FixT (Leaf_ x)) = "Leaf_ " ++ (show x)
```
Thus far, i have looked at walking the data structures and understanding how to handle
nested recursive data types and it's time to look at the reverse. This process of reversing
the folding approach is called unfolding. Below, i focused on creating List and Tree where the
nested values are numbers.
```haskell
unfoldList :: (Num a, Eq a) => a -> Fix List_ a
unfoldList 0 = FixT Nil_
unfoldList n = FixT (Cons_ n (unfoldList (n-1)))

unfoldTree :: Num a => [a] -> Fix Tree_ a -- right-leaning tree.
unfoldTree []  = FixT (Leaf_ (-1))
unfoldTree [x] = FixT (Leaf_ x)
unfoldTree (x:y:xs) = FixT (Node_ x (unfoldTree [y]) (unfoldTree xs))
```
The fact that unfoldTree is a right-leaning tree does not affect our learning of this pattern.
When it comes to folding & unfolding structures like this, it makes a lot of sense to consider
data structures that allows me to apply functions when traversing data values from the data types
like `Tree_ a r` and `List_ a` rand turns out `Data.Bifunctor` is such a type-class which allows
us to do that. Let's take a quick look at how such code can be written
```haskell
class Bifunctor (p :: * -> * -> *) where
 bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
 first :: (a -> b) -> p a c -> p b c
 second :: (b -> c) -> p a b -> p a c

instance Bifunctor List_ where
  bimap f g Nil_        = Nil_
  bimap f g (Cons_ a r) = Cons_ (f a) (g r)

instance Bifunctor Tree_ where
  bimap f g (Leaf_     a) = Leaf_ (f a)
  bimap f g (Node_ a l r) = Node_ (f a) (g l) (g r)

genericFold :: Bifunctor s => ( s a b -> b ) -> Fix s a -> b
genericFold f = f . bimap id (genericFold f) . getFix

```
The function genericFold deserves a more in-depth study and what is written here is that i need to
get the nested structure and hence `getFix`  is the first action to be performed; next, given this
nested structure i can apply the `bimap` to it where i'm focusing into digging into the nested
structure and once the innermost shape is revealed, then we ask `f` to apply to it. Now, we are going
to walk through a more complete example …
```haskell
addL :: Num a => List_ a a -> a
addL (Cons_ x r) = x + r
addL Nil_        = 0

aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))

g = genericFold addL aListF -- g gives '25'
-- let's walk through; conceptually 
-- f . bimap id (genericFold f) $ (Cons_12 ...)
-- f . bimap (id Cons_ 12) (genericFold f (FixT (Cons_ 13 (FixT Nil_))))
-- .... skipping 
-- (f 12 (f 13 (f Nil_)))
-- 12 + 13 + 0
```
Let's try to map a function against the list and finally adding the entire list, let's look at this
code snippet and the key insight here is that once you have defined nested recursive structure in
terms of Fix then the r part (i.e. recursive part) will be taken care of by the machinery and this
is evident in the function `mapL'` :
```haskell
mapL' :: (t -> a) -> List_ t (Fix List_ a) -> Fix List_ a
mapL' f (Cons_ x xs) = FixT (Cons_ (f x) xs)
mapL' f Nil_         = FixT Nil_
f = genericFold addL $ genericFold (mapL' (+1)) aListF
-- f gives '27'
```
Having explored the folding approach in a recursion scheme, it's time back to double back and see how the unfolding approach. 
```haskell
genericUnfoldL :: Bifunctor s => (b -> s a b) -> b -> Fix s a
genericUnfoldL f = FixT . bimap id (genericUnfoldL f) . f
```
Yes, we can reuse the `bimap` and applied the rationale but this time, in reverse. The critical part
about understanding this function is to define the `f` function properly because this is where you have
to define how the values are to be encapsulated into the `List_ a r` structure (see the function `toList`)and i have the following code snippet and example
```haskell
toList :: (Eq r, Num r) => r -> List_ r r
toList 0 = Nil_ 
toList n = (Cons_ n (n - 1))

putStrLn . showListF $ genericUnfoldL toList 10
-- 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, Nil_
```
Let's get a little more creative and pass a predicate function that when it predicate passes, it would
continue carrying out the unfolding of a list otherwise it would return `Nil_` to signal that the construction
has ended. With this new function `toList'` let's have a look at how we can construct the list.
The point of this example is to demonstrate that once we figured out the fix-point data structure, we can
other functions in Haskell that perform like any other we know of with the caveat that i keep a constant
reminder of which parts are recursing .
```haskell
toList' :: (Eq r, Num r) => (r -> Bool) -> r -> List_ r r
toList' f n = if f n then Nil_ else (Cons_ n (n - 1))
putStrLn . showListF $ genericUnfoldL (toList' (< (-10))) 10
-- 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, Nil_
```

## Put them all together (i.e. (fold . unfold)[+1] == hylomorphism)
The term hylomorphism was used to describe the process of unfolding and then folding. A useful to illustrate this is 
```
fold . unfold (fold . unfold ( fold . unfold …) )
```
and concretely, in code (of course), as follows
```haskell
-- fold . unfold
genericFold addL (genericUnfold toList 100) -- 5050
-- fold . unfold (fold . unfold)
genericFold addL (genericUnfold toList (genericFold addL (genericUnfold toList 100))) -- 12753775
-- fold . unfold (fold . unfold)
genericFold addL \
  (genericUnfold (toList' (< (-10))) \
  (genericFold addL (genericUnfold (toList' (< (-10))) 100)
)) -- 12477455
-- just keeps folding and unfolding etc 
-- ...
```
The above code snippet illustrates a general idea and that is folding and unfolding is theoretically infinite like the Turing Machine.
As it turns out, the term hylomorphism was mentioned in the paper Datatype-Generic Programming by Jeremy Gibbons.
The wikipedia says that it is basically a composition between a cata-morphism (i.e. folding) and a ana-morphism (i.e. unfolding).
There are other patterns described in that paper but my personal favourite is hylomorphism as its quite intuitive.


## Conclusion

I definitely think that this origami pattern (that's what some Haskellers are calling it and having done some basics of it,
i can appreciate why) of writing code is helpful and it does reduce boilerplate code in my own projects; admittedly you
get this kind of productivity only after realising how the code can be structured and when it'll take time to gain mastery.
A point along the "journey to mastery" is that it definitely needs time and practice to get it right and i'll argue that
it reduces readability (when you don't get it right) but it's certainly very clever to write code this way.

