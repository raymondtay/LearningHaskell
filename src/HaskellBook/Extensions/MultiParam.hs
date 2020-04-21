{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


class Vector v where
  distance :: v -> v -> Double

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

main :: IO ()
main = do
  putStrLn "Hello, World!"

