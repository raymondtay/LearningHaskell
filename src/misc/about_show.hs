
data A = A Int

data B = B Int

data C a = C a

instance Eq A where
  A x == A y = x == y
instance Eq B where
  B x == B y = x == y


instance Show A where
  show (A x) = show x
instance Show B where
  show (B x) = show x

instance (Show a) => Show (C a) where
  show (C a) = show a 

data Tr = Tr
instance Eq Tr where
  Tr == Tr = True

