# Monad Readers

On page 872, i quote:

"""

This is the idea of the Reader. It is a way of stringing functions together
when all those functions are awaiting one input from a shared environment. We
are going to get into the details of how that works, but the important
intuition here is that it is just another way of abstracting out function
application and gives us a way to do computation in terms of an argument that
has not been supplied yet. We use this most often when we have a constant value
that we will obtain from somewhere outside our program that will be an argument
to a whole bunch of functions. Using Readers allows us to avoid passing that
argument around explicitly.

"""

# Functions have an Applicative too

The first thing we want to do is notice how the types specialize:
```haskell

-- Applicative f => 
-- f ~ (->) r

pure :: a -> f a
pure :: a -> (r -> a)

(<*>) :: f ( a -> b ) -> f a -> f b
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

```

As we saw in the Functor isntance, the `r` of Reader is part of the `f`
structure. We have two arguments in this function, and both of them are
functions waiting for the `r` input. When that comes, both functions will be
applied to return a final result of `b`.

