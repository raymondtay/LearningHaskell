
mytails :: [a] -> [[a]]

mytails (x:xs) = xs : mytails xs
mytails []     = []

