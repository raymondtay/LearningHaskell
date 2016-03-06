
data FixMePls a = 
  FixMe a 
  | Pls a deriving (Eq, Show)

instance Functor FixMePls  where
  fmap f (FixMe a) = Pls (f a)
  fmap f (Pls a) = FixMe (f a)

a = fmap (++ "lol") $ (\f -> case f of (Just xs) -> xs) (Just["hi,", "world"])
b = fmap (+1) $ read "[23]" :: [Int]
c = fmap (*2) (\x -> x -2 ) $ 1
d = fmap ((return '1' ++ ) . show) (\x -> [x,1..3]) $ 0

data Two a b = Two a b deriving (Show,Eq)

data Or' a b = First a | Second b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or' a) where
  fmap _ (First  a ) = First a
  fmap f (Second b) = Second (f b)

