
Here's 1 way to write infinite lists

> myiterate :: (a -> a) -> a -> [a]
> myiterate f x = x : iterate f (f x)

Here's 1 way to write the pythagoras formulae

> triads :: Int -> [(Int, Int, Int)]
> triads n = [(x, y, z) | x <- [1..n],
>                         y <- [1..n],
>                         z <- [1..n], x*x + y*y == z*z]

> divisors x = [d | d <- [2..x-1], x `mod` d == 0]

 coprime x y = disjoint (divisors x) (divisors y)
 disjoint 
