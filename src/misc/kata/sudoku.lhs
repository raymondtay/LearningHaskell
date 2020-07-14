
> type Matrix a = [Row a]
> type Row a    = [a]
> type Grid = Matrix Digit
> type Digit = Char
> digits :: [Char]
> digits = ['1'..'9']
> blank :: Digit -> Bool
> blank = (== '0')
> solve :: Grid -> [Grid]
> solve = filter valid . completions

> completions :: Grid -> [Grid]
> completions = expand . choices

> valid :: Grid -> Bool
> valid g = all nodups (rows g) &&
>           all nodups (cols g) &&
>           all nodups (boxs g)

> nodups :: (Num a) => [a] -> Bool
> nodups [] = True
> nodups (x:xs) = all (/=x) xs && nodups xs

> rows :: Matrix a -> Matrix a
> rows = id

`cols` computes the transpose of a matrix. Thus, if a matrix
consists of m rows where each row has length n, the transpose of a
list of n rows where each row has length m.

> cols :: Matrix a -> Matrix a
> cols [xs] = [[x] | x <- xs]
> cols (xs:xss) = zipWith (:) xs (cols xss)

> boxs :: Matrix a -> Matrix a
> boxs = map ungroup . ungroup . map cols . group . map group
> group :: [a] -> [[a]]
> group [] = []
> group xs = take 3 xs:group (drop 3 xs)
> ungroup :: [[a]] -> [a]
> ungroup = concat

If the cell is blank, then all digits are installed as possible choices
otherwise there is only one choice and a singleton is returned. If we want to
apply f to every element of a matrix then map (map f) is the function to use
because after all a matrix is just a list of lists.

> cartesian (xs:xss) = [x:ys | x <- xs, ys <- yss] where yss = cartesian xss
> cartesian [] = [[]]

> choices :: Grid -> Matrix [Digit]
> choices = map (map choice)
> choice d = if blank d then digits else [d]

> expand :: Matrix [Digit] -> [Grid]
> expand = cartesian . map cartesian

