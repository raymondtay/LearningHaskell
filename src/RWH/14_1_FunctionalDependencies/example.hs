
-- The is lifted (most of it anyway) from 
-- https://wiki.haskell.org/Functional_dependencies#Tutorials
-- 
data Vector = Vector Int Int deriving (Show, Eq)

data Matrix = Matrix Vector Vector deriving (Eq, Show)


instance Num Vector where
  Vector a b + Vector x y = Vector (a + x) (b + y)
  Vector a b - Vector x y = Vector (a - x) (b - y)
  Vector a b * Vector x y = Vector (a * x) (b * y)
  abs (Vector a b) = Vector (abs a) (abs b)
  signum (Vector a b) = undefined -- i dont care about this for now, this is a toy program
  fromInteger x = Vector (abs $ fromInteger x) (abs $ fromInteger x) -- i dont care about this for now, this is a toy program

instance Num Matrix where
  Matrix a b + Matrix x y = Matrix (a+x) (b+y)
  Matrix a b - Matrix x y = Matrix (a-x) (b-y)
  Matrix a b * Matrix x y = Matrix (a*x) (b*y)
  abs (Matrix a b) = Matrix (abs a) (abs b)
  signum (Matrix a b) = undefined -- i dont care about this for now, this is a toy program
  fromInteger x = Matrix (Vector (fromInteger x) (fromInteger x))(Vector (fromInteger x) (fromInteger x)) -- i dont care about this for now, this is a toy program

{-

The problem comes whenyou want to start multiplying quantities. You really need a multiplication function which overloads to a different types:

(*) :: Matrix -> Matrix -> Matrix
(*) :: Matrix -> Vector -> Vector
(*) :: Matrix -> Int -> Matrix
(*) :: Int -> Matrix -> Matrix
  {- ... and so on ... -}

-}

