{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- The Haskell Report allows instance declarations only for types whose shape
-- is a name followed by a list of distinct type variables. The previous
-- definition doesn't follow that lead, so the compiler complains. However, GHC
-- supports those declarations if you enable the FlexibleInstances extension.
--
class Vector v where
  distance :: v -> v -> Double

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)

main :: IO ()
main = do
  putStrLn "Hello, World!"

