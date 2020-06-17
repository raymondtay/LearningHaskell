{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Article of inspiration: http://wiki.haskell.org/Functional_dependencies
--
-- Suppose you want to implement some code to perform simple linear algebra:
--
data Vector = Vector Integer Integer deriving (Eq, Show)
data Matrix = Matrix Vector Vector deriving (Eq, Show)

instance Num Vector where
  Vector a1 b1 + Vector a2 b2 = Vector (a1 + a2) (b1 + b2)
  Vector a1 b1 * Vector a2 b2 = Vector (a1 * a2) (b1 * b2)
  Vector a1 b1 - Vector a2 b2 = Vector (a1 - a2) (b1 - b2)
  abs (Vector a1 b1) = Vector (abs a1) (abs b1)
  fromInteger a = Vector (abs a) (abs a)
  signum (Vector a b) = Vector (abs a) (abs b)

instance Num Matrix where
  Matrix v1 v2 + Matrix v3 v4 = Matrix (v1 + v3) (v2 + v4)
  Matrix v1 v2 * Matrix v3 v4 = Matrix (v1 * v3) (v2 * v4)
  Matrix v1 v2 - Matrix v3 v4 = Matrix (v1 - v3) (v2 - v4)
  abs (Matrix v1 v2) = Matrix (abs v1) (abs v2)
  fromInteger a = Matrix (Vector a a) (Vector a a)
  signum (Matrix a b) = Matrix (abs a) (abs b)


-- With no relationship defined between type parameters 'a', 'b' and 'c' we are
-- able to write the instances like the ones we find below.
class Mult a b c where
  (**) :: a -> b -> c

-- this makes sense right?
instance Mult Vector Vector Vector where
  (Vector a1 b1) ** (Vector a2 b2) = Vector (a1 * a2) (b1 * b2)

-- question is whether this makes sense ?
instance Mult Vector Vector Integer where
  (Vector a1 b1) ** (Vector a2 b2) = (a1 * a2) * (b1 * b2)


class Mult2 a b c | a b -> c where -- it means the type parameter 'a' and 'b' uniquely defines 'c'
  (*|*) :: a -> b -> c

-- Using functional dependencies, we allow this
instance Mult2 Vector Vector Vector where
  (Vector a1 b1) *|* (Vector a2 b2) = Vector (a1 * a2) (b1 * b2)

-- this kind of declaration is disallowed
-- instance Mult2 Vector Vector Integer where
  -- (Vector a1 b1) *|* (Vector a2 b2) = (a1 * a2) * (b1 * b2)

