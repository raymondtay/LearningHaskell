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


