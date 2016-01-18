{-# LANGUAGE TemplateHaskell #-}

Writing a typeclass tutorial. We start by developing 
a typeclass "Trivial" 

data Trivial = Trivial
Trivial == Trivial

At this point in time, GHC cannot find an instance of `Eq` for our datatype Trivial.
We could have GHC generate one for us by deriving Eq or we could've written
one, but we did neither, so none exists and it fails at compile time.

So, we must write our own! Fortunately, with Trivial this is ... trivial ;)

> data Trivial = Trivial'
> instance Eq Trivial where
>   Trivial' == Trivial' = True
let x = Trivial', y = Trivial' in x == y


