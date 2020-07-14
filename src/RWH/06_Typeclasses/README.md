## About Typeclasses in Haskell

Typeclasses defines a set of functions that can have different implementations
depending on the type of data they are given. Typeclasses may look like the 
objects of object-oriented programming but they are truly quite different.

### When to use the OverlappingInstances extension

According to the book:
<pre>
Here's an important point: GHC treats OverlappingInstances as affecting
the declaration of an instance, not a location where we use the instance.
In other words, when we define an instance we wish to allow
to overlap with another instance, we must enable the extension for the
module that contains the definition. When it compiles the module,
GHC will record that instance as "can beoverlapped with other instances"

Once we import this module and use the instance, we won't need to
enable OverlappingInstances in the importing module. GHC will already
know that the instance was marked as "Ok to overlap" when it was defined. 

This behavior is useful when we are writing a library: we can choose to 
create overlappable instances, but users of our library do not need to
enable any special language extensions.
</pre>

## Understanding and implementing Type classes in Haskell

The following passage is lifted from [Implementing and Understanding Type Classes](http://okmij.org/ftp/Computation/typeclass.html)

Computational abstractions - higher-order functions, continuations, modules, processes and automatic memory 
management have made programs much faster to write, easier to demonstrate correctness and improve code reusability.
And yet there is often subconscious resistence to abstractions: they appear ritualistic, formal -- too abstract. 
One gets the feeling of getting lost. To overcome the mistrust for an abstraction it may help to look at its
realization, to see what is being abstracted away. The awareness of low-level implementation details brings 
the appreciation of an abstraction and the intuitive explanation for it.

Now we look at behind the scenes of the abstraction of parameteric overloading a.k.a bounded polymorphism or just
"type classes". Seeing the implementation makes type classes appear simpler, friendlier, 
and more comfortable ot use. The types and type class definitions ar no longer incantations to memorize: they 
suddenly make sense. Knowing what tedious job GHC is doing for us helps us appreciate more the 
convenience of type classes. 

Dictionary passing, although best known, is not the only compilation strategy for type classes. Historically, first
were static specialization and run-time resolutions, introduced by the father of parameter overloading, Stefan Kaes.
He presented the type system and proved its soundess, descirbe the type inference algorithm na dproved the soundness
and consistency of the two implementation of what is now known as type classes. IT is shameful that his name is almost 
forgotten, his strategies are still in wide use howevr. Local type classes and instances introduced in his paper still
await recognition. 

Dictionary passing makes it easier, in retrospect, to under the other two implementation strategies. Therefore, we
descrieb it first and in detail.WE explain by example, juxtaposing Haskell code with the 
corresponding code in a language with no type classes (OCaml). The implementation language could be any other 
higher-order language, including the GHC COre. For the sake of explanation, we restrict ourselves to 
the single-parameter, non-constructor type classes such as "Num", "Eq" and "Show". The other two implementation
strategies are presented next, illustrating the algorithms from the Kaes's paper, in modern terms.

## The laws of functors in Haskell
If we look at the implementation of fmap for, say Maybe, we can figure out why the first functor law holds:
<pre>
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing  = Nothing
</pre>
We imagine that "id" plays the role of the "f" parameter in the implementation. We see that if we 
"fmap id" over "Just x", the result will be "Just (id x)" and because id is the identity function
we can decide that "Just (id x)" returns the "Just x". So now we know that if we map id 
over a "Maybe" value with a "Just" value constructor, we get that same value back.

Seeing that mapping "id" over a "Nothing" value returns the same value is trivial. So from these two equations
in the implementation for "fmap", we see that the law "fmap id = id" holds. 

The second law says that composing two functions and then mapping the resulting function over a 
functor should be the same as first mapping one function over the functor and then mapping the 
other one. Formally written, that means that 
"fmap (f . g) = fmap f . fmap g" or to write it in another way, for any functor F the following should hold:
"fmap (f . g) F = fmap f (fmap g F)"

If we can show that some type obeys both functor laws, we can rely on it having the same fundamental behaviors as
other functors when it comes to mapping. We can know that when we use fmap on it, there won't be anything other than
mapping going on behind the scenes and that it will act like a thing that can be mapped over i.e. a functor

If you want, we can check out how the second functor law holds for "Maybe". If we do 
fmap (f . g) over Nothing, we get Nothing, because doing a fmap with any function over Nothing returns Nothing.
If we do fmap f (fmap g Nothing) we get Nothing, for the same reason. OK, seeing how the second 
law holds for Maybe if it's a Nothing value is pretty easy, almost trivial.

How about if it's Just something vlaue? Well, if we do fmap ( f . g) (Just x) and referencing back to the 
the implementation of Functor we see that its just Just ((f . g ) x) and then Just (f (g x)); conversely
if we flip the coin around and try using fmap which equates to fmap f (fmap g (Just x)) then again from the
previous implementation, its just (fmap f (Just (g x)))

## About Applicative Functors

The "Applicative" typeclass holds the idea of what it is to be a applicative functor.

Functions in Haskell are curried by default, which means that a function
that takes several parameters actually takes just one parameter and returns
a funciton that takes the next parameter etc. E.g. if a funciton is of type
f :: a -> b -> c then currying means that f(a) returns a function of the type (b -> c)
i.e. f(a) :: b -> c 

So far, when were mapping functions over functors, we usually mapped functions that take 
only 1 parametr but what if when we need to map a function like * which takes two parameters
over a functor ? For example let's consider the example below

fmap (*) (Just 3) ??

based on the implementation, we know that 
fmap (*) (Just 4) = Just ((*) 4)
and that works out to be of type Maybe (a -> a) where a :: Int loosely speaking.

We see how by mapping "multi-parameter" functions over functors, we get functors that contain 
functions inside them. So now what can we do with them ? Well for one, we can map functions that take
these functions as parameters over them, because whatever is inside a functor will be given to the function
that we are mapping over it as a parameter.

> let a = fmap (*) [1, 2, 4, 5]
> :t a
a :: [Integer -> Integer]
> fmap (\f -> f 9) a
[9, 18, 36, 45]

and you can see that fmap literally allows a set of partially applied functions to be residing
in a container like structure i.e. [Integer -> Integer] and then we scan the data structure again
to apply values on that list of functions. However, if we were to step back a little and think about this
again, we notice that fmap literally only allows us to go 1-level deep and if we had n-levels...fmap would
not suffice to handle this scenario and that's where Applicative typeclass comes in.

The haskell Control.Applicative defines two methods: pure and <*> and doesn't provide any default
implementation for either of them and looks like this:

> class (Functor f) => Applicative f where
>     pure :: a -> f a
>     (<*>) :: f (a -> b) -> f a -> f b
> 
"pure" basically is a "init"-like thing => you give it something and it returns a functor with that something
"<*>" takes a functor and a function and ... you might notice that it resembles how fmap is defined i.e.
> fmap :: Functor f => (a -> b) -> f a -> f b
and <*> allows us to sort of recursively apply (a -> b) across the functors found and in that sense
its a beefed up version of fmap. Caveat emptor: that's how i understood it.

> instance Applicative Maybe where 
>     pure v = (Just v)
>     Nothing <*> _ = Nothing
>     (Just f) <*> somethingelse = fmap f somethingelse

