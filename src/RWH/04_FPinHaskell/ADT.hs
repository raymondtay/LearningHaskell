
data Tree a = Tip a  | Fork (Tree a) (Tree a)
instance Functor Tree where
  fmap f (Tip x ) = Tip (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

instance Show (Tree a) where
  show (Tip a) = show a
  show (Fork l r) = show l ++ show r
