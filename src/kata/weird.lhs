Here's something from http://en.wikibooks.org/wiki/Haskell/Other_data_structures
and i thought it was a fun thing to do and since i'm a beginner in Haskell
it is always very prudent to keep coding and learning ☺

> data Weird a b  = First a
>   | Second b
>   | Third [(a, b)]
>   | Fourth (Weird a b)

> weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
> weirdMap f g (First a) = First (f a)
> weirdMap f g (Second b) = Second (g b)
> weirdMap f g (Third [(a, b)]) = Third [(f a, g b)]
> weirdMap f g (Fourth (First a))       = (Fourth (First (f a)))
> weirdMap f g (Fourth (Second b))      = (Fourth (Second (g b)))
> weirdMap f g (Fourth (Third [(a, b)]))= (Fourth (Third [(f a, g b)]))

> weirdMap2 f g = h where
>     h (First a)  = First (f a)
>     h (Second b) = Second (g b)
>     h (Third [(a, b)]) = Third[(f a, g b)]
>     h (Fourth(First a))       = Fourth(First (f a))
>     h (Fourth(Second b))      = Fourth(Second (g b))
>     h (Fourth(Third [(a, b)]))= Fourth(Third[(f a, g b)])
>     
