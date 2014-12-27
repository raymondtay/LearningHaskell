
-- a simple demonstration of writing higher-order functions using `folds`.

productViaFoldl = foldr1 (*)

filterViaFoldl p = foldr (\x acc -> if p x then x : acc else acc) []

headViaFoldl = foldr1 (\x _ -> x)

lastViaFoldl = foldl1 (\_ x -> x)

reverseViaFoldl xs = foldl (\acc x -> x : acc) [] xs

