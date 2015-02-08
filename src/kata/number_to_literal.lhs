> units, teens, tens :: [String]
> units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
> teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
> tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

simplest version of how we could determine if a number, n, contains only one digit

> convert1 :: Int -> String
> convert1 n = units !! n

next simplest verion of determining when the number n has up to two digits

> digits2 :: Int -> (Int, Int)
> digits2 n = (div n 10, mod n 10)

now we can define 

> convert2 :: Int -> String
> convert2 = combine2 . digits2
> combine2 :: (Int, Int) -> String
> combine2 (t, u)
>   | t == 0 = units !! u
>   | t == 1 = teens !! u
>   | 2 <= t && u == 0 = tens !! (t -2 )
>   | 2 <= t && u /= 0 = tens !! (t -2 ) ++ "-" ++ units !! u

another way to write `combine2`

> convert2_1 :: Int -> String
> convert2_1 n 
>   | t == 0 = units !! u
>   | t == 1 = teens !! u
>   | u == 0 = tens !! (t -2)
>   | otherwise = tens !! (t -2) ++ "-" ++ units !! u
>   where (t, u) = (div n 10, mod n 10)

now, we can write another function to decipher 3-digit numbers

> convert3 :: Int -> String
> convert3 n
>   | h == 0    = convert2 t
>   | n == 0    = units!!h ++ " hundred"
>   | otherwise = units !! h ++ " hundred and " ++ convert2 t
>   where (h, t) = (div n 100, mod n 100)

> convert6 :: Int -> String
> convert6 n
>   | h == 0    = convert3 t
>   | t == 0    = convert3 h ++ " thousand"
>   | otherwise = convert3 h ++ " thousand" ++ link h ++ convert3 t
>   where (h, t) = (div n 1000, mod n 1000)

> link :: Int -> String
> link h = if h < 100 then " and " else " "

> roots :: (Float, Float, Float) -> (Float, Float)
> roots (a, b, c)
>   | a == 0    = error "not quadratic"
>   | disc < 0  = error "complex roots"
>   | otherwise = ((-b -r)/e, (-b+r)/e)
>   where { disc = b*b - 4*a*c; r = sqrt disc; e = 2*a }

> showDate :: Date -> String
> showDate (d, m, y) = show d ++ suffix d ++ " " ++ months !! (m-1) ++ ", " ++ show y
 
