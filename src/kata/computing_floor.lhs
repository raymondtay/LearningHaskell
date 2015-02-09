
A better method for computing `floor` is to first find integers
m and n such that m <= x < n and then drink the interval (m,n) s.t.
m + 1 == n that contains x. Then the left-hand buond of the interval can be returned
as the result. That leads to 

> floor :: Float -> Integer
> floor x = fst (until unit (shrink x) (bound x))
>       where unit (m, n) = (m + 1 == n)

the `bound x` is some pair (m,n) of integers s.t. m <= x < n
if (m,n) is not a unit interval, then `shrink x (m, n)` returns a
new interval of strictly smaller size that still bounds x.

> leq :: Integer -> Float -> Bool
> leq x = \y -> x == y || x < y

> type Interval = (Integer, Integer)
> shrink :: Float -> Interval -> Interval
> shrink x (m, n) = if p `leq` x then (p, n) else (m, p) 
>   where p = choose (m, n)

How do we define `choose ...` ?? We can either do `choose (m, n) = m + 1`
or `choose (m, n) = n - 1` for both reduce the size of an interval. 
but appparently, according to richard bird he says that division-by-2 is a better choice.
And this choice actually makes sense since this the basic principle of binary-search

> choose :: Interval -> Integer
> choose (m, n) = (m + n) `div` 2

> bound :: Float -> Interval 
> bound x = (lower x, upper x)

> lower :: Float -> Integer
> lower x = until (`leq` x) (*2) (-1)
> lt x = \y -> (< x y)
> upper :: Float -> Integer
> upper x = until (x `lt`) (*2) (1)

