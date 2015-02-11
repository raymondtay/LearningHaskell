
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

determining whether 2 numbers have anything in-common

> coprime x y = disjoint (divisors x) (divisors y)

the real function that determines whether there are anything in-common

> disjoint [] _ = False
> disjoint _ [] = False
> disjoint xs ys =
>     case length xs `compare` length ys of
>         GT -> scan xs ys
>         LT -> scan ys xs
>         _  -> scan ys xs -- doesn't matter which one its going to be 
>     where scan longerlist shorterlist = any (\a -> a == True) $ map (\e -> elem e longerlist) shorterlist

determine whether the numbers in the given list is in ascending order

> nondec :: (Ord a) => [a] -> Bool
> nondec [] = True
> nondec [x] = True
> nondec (x:y:xs) = (x <= y) && nondec (y:xs)

> nondec2 xs = and (zipWith (<=) xs (tail xs))

an example of how to locate the position of the element, `x`, in 
the list, `xs`.

> position :: (Eq a) => a -> [a] -> Int
> position x xs = head ([a | (a, b) <- zip [0..] xs, x == b] ++ [-1])

