
> expn :: Num a => a -> Nat -> a
> expn x Zero = 1
> expn x (Succ n) = x * expn x n

> data Nat = Zero | Succ Nat

> ipart :: String -> Integer
> ipart xs = read( takeWhile (/= '.') xs):: Integer

> fpart :: String -> Float
> fpart xs = read ('0' : dropWhile (/= '.') xs) :: Float

> parts :: String -> (Integer, Float)
> parts ds = (ipart2 is, fpart2 fs)
>   where   (is, fs) = break (== '.') ds
>           ipart2 = foldl shiftl 0 . map toDigit where shiftl n d = n*10 + d
>           fpart2 = foldl shiftr 0 . map toDigit where shiftr d x = (d + x) / 10
>           toDigit d = fromIntegral (fromEnum d - fromEnum '0')


