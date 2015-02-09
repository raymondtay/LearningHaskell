
Here's 1 way to write infinite lists

> myiterate :: (a -> a) -> a -> [a]
> myiterate f x = x : iterate f (f x)

Here's 1 way to write the pythagoras formulae

> triads :: Int -> [(Int, Int, Int)]
> triads n = [(x, y, z) | x <- [1..m],
>                         y <- [x+1..n], coprime x y,
>                         z <- [y+1..n], x*x + y*y == z*z]
>   where m = floor (fromIntegral n / sqrt 2)

> divisors x = [d | d <- [2..x-1], x `mod` d == 0]

> coprime x y = disjoint (divisors x) (divisors y)
> disjoint [] _ = False
> disjoint _ [] = False
> disjoint xs ys =
>     case length xs `compare` length ys of
>         GT -> scan xs ys
>         LT -> scan ys xs
>         _  -> scan ys xs -- doesn't matter which one its going to be 
>     where scan longerlist shorterlist = any (\a -> a == True) $ map (\e -> elem e longerlist) shorterlist
