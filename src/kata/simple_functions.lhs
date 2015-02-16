> import Control.Monad.Writer

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

classic divide-and-conquer approach to devising a sorting algorithm

> sort :: (Ord a) => [a] -> [a]
> sort [] = [] 
> sort [x] = [x]
> sort xs = merge (sort ys) (sort zs) where (ys, zs) = halve (xs)
> halve xs = (take n xs, drop n xs) where n = length xs `div` 2
> merge [] ys = ys
> merge xs [] = xs
> merge (x:xs) (y:ys)
>     | x <= y = x : merge xs (y:ys)
>     | otherwise = y : merge (x:xs) ys

a more efficient `merge` function can be written as follows

> merge2 [] ys' = ys'
> merge2 xs' [] = xs'
> merge2 xs'@(x:xs) ys'@(y:ys)
>     | x <= y = x : merge xs ys'
>     | otherwise = y : merge xs' ys
   
You want to produce an infinite list of all distinct pairs (x, y) of natural 
numbers. It does matter in which order the pairs are enumerated, as long as they are all here. 

> allPairs = [(x, y) | x <- [0..], y <- [0..]]

The pairs (0,0) (1,1) ... where (x, x) are not filtered, so a better solution looks to be

> allPairs2 = [(x, y) | x <- [0..], y <- [0..] , x /= y]

Another problem presents itself here. Notice that [0..] produces an infinite list 
and that means that there is not going to be any chance in hell that we'll see pairs that 
don't begin with 0. The crux is recognizing that if the list-comprehension for `y` doesn't 
end, neither does `x`.

> allPairs3 = [(x, y) | x <- [0..], y <- [0..x], x /= y]

Another interesting problem is to find the solution to 
a^3 + b^3 = c^3 + d^3. An example would be 1729 = 1^3 + 12^3 = 9^3 + 10^3

> quads n = [(x,y) | x <- [1..n] , y <- [1..n],x^3 + y^3 == n]

When you give an idx that is too large for the list `l`, 
then it'll trigger the `fail` fn in the Monad since `l` is a Monad too

> fn :: Int -> Maybe [Int]
> fn idx = do
>   let l = [Just [1,2,3], Nothing, Just [], Just [7..20]]
>   (x:xs) <- l !! idx
>   return xs

the function `>>` is quite interesting and is defined like this 
(>>) :: m a -> m b -> m b
m >> k = m >>= (\_ -> k)

when used in the following way, it behaves like `snd`

> return 4 >> [1..2] -- returns [1,2]

> [1..10] >> [1..2] -- returns [1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2]

we can understand the latter expression by the following 
where [1..10] >> [1..2] becomes [1..10] >>= (\_ -> [1..2])
and we can write the above using the following 
concat $map (\_ -> [1..2]) [1..10]


> inits [] = [[]]
> inits (x:xs) = [] : map (x:) (inits xs)

The expression `myscanl (+) 0 [1..10]` computes the running sums of the
first ten positive numbers but there's a problem and that is the computation
process involves evaluating `f` a total of 

0 + 1 + 2 + ... + n = n(n + 1) / 2 times.

> myscanl :: (b -> a -> b) -> b -> [a] -> [b]
> myscanl f e = map (foldl f e) . inits

Another way to do this is 

> myscanl2 f e []     = [e]
> myscanl2 f e (x:xs) = e : myscanl f (f e x) xs

Maximum segment sum, which is the maximum sum of all segments in the sequence. 
A segment is also called a contiguous subsequence. For example, the sequence 

[-1, 2, -3, 5, -2, 1, 3, -2, -2, -3, 6]

has maximum sum 7 given by the sum of the segment [5, -2, 1, 3]. OTOH 
the sequence [-1, -2, -3] has a maximum segment sum of zero since the
empty sequence is a segment of every list and its sum is zero.
It follows that the maximum segment is always nonnegative.

> mss :: [Int] -> Int
> mss = maximum . map sum . segments

where segments returns a list of all segments of a list.We can define like
the following

> segments :: (Num a) => [a] -> [[a]]
> segments = concat . map inits . tails

> tails :: (Num a) => [a] -> [[a]]
> tails []     = [[]]
> tails (x:xs) = (x:xs) : tails xs

