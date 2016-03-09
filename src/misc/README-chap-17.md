
# Difference btween fmap and `(<*>)`
```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

The difference appears to be quite small and innocuous.
We now have an f in front of our function (a -> b). But the
increase in power it introduces is profound. For one thing, any
Applicative also has a Functor and not merely by definition - 
you can define a Functor in terms of a provided 

