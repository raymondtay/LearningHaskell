module Chapter16_1 where

data CountingBad a = Heisenberg Int a deriving (Eq,Show)

-- This compiles and is going to be very bad.
-- 
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

data Two a b = Two a b deriving (Eq, Show)  

data Or a b = First a | Second b deriving (Eq, Show)

-- 
-- The question you want to ask yourself is probably this:
-- "How the hell does it actually work?" and to answer this question, you need
-- to really understand the relationship between types and values
--
-- To understand this relationship, you want to understand what the type
-- signature is supposed to look like....
--
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (Two a) bound to 'f' => (a -> b) -> f (Two x b) -> Two (x (f b))
--
-- The type parameter 'x' is part of the functorial structure of 'f' and it is
-- basically untouchable.
-- 
-- In the following definition,
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- here's an example of how to use this: 
-- > fmap (+1) (Yeppers 4)
-- > Yeppers 5
-- 
-- remember that the intention of the functor
-- is to lift a function into the Yeppers value and here's another example of
-- this in action
--
-- > fmap show (fmap (+1) (Yeppers 4))
-- > Yeppers "5"
--




