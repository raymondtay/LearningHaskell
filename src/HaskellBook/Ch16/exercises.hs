{-# LANGUAGE FlexibleInstances #-}

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

-- 
-- Due to my poor organization of the code, the names
-- are repeated so i've appended 't' to the value constructors
--
data Sum a b = Firstt a | Secondt b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Firstt a) = Firstt a
  fmap f (Secondt b) = Secondt (f b)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant d) = (Constant d)

data Wrap f a = Wrap (f a) deriving (Eq,Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- doesn't work.
--instance Show (Wrap f a) where
  --show wfa = "function: " ++ show(wfa)

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- data K a b = K a
-- instance Functor (K a) where
-- fmap _ (K a) = K a


-- The syntax Flip (f b a) actually turns out to be equivalent to
-- Flip $ (+1) 2 OR
-- Flip ((+) 1 2) 
-- 3 which is actually of the type `Flip f a b`.
--
newtype K a b = K a
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b)) 

-- To understand the snippet above, it is helpful to understand the kind
-- signature:
-- *Chapter16_1> :k K
-- K :: * -> * -> *
-- *Chapter16_1> :k Flip
-- Flip :: (* -> * -> *) -> * -> * -> *
-- 
-- From the above, we realize that (Flip K) would give us 
-- *Chapter16_1> :k Flip K
-- Flip K :: * -> * -> *
-- *Chapter16_1> :k Functor
-- Functor :: (* -> *) -> Constraint
--
-- And from here, we know that for (Flip K) to be a valid Functor, we need to
-- provide a type parameter, say 'a'. Then after that, we are good to apply the
-- usual understanding of how to define `fmap` for Flip K instances.
--
