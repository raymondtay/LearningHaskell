{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck

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

-- 
-- fmap id = id
-- fmap (f . g ) = fmap f . fmap g
--
-- Some interesting stuff that quickcheck can help 
-- to test the validate the laws
--
functorCompose :: (Eq (f c), Functor f) =>
                     (a -> b)
                  -> (b -> c)
                  -> f a 
                  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- QuickCheck happens to offer the ability to generate functions. There's a
-- different but related typeclass called CoArbitrary, this covers the function
-- argument type where Arbitrary is used for the function result type. If you are 
-- curious about this, take a look at the Function module in the QuickCheck
-- library to see how functions are generated from a datatype that represents 
-- patterns in function construction.
--
-- Implemented Functor instances for the following data types. USe the
-- QuickCheck properties we just showed you to validate them.
--
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- need to declare this generator to allow quickcheck
-- know how to generate 'Int' in its tests.
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

id_f x = (fmap id x) == x 
id_check = quickCheck (id_f :: (Identity Int) -> Bool)
id_compose_f x = functorCompose (\a -> a == a) (\b -> b == b) x
id_compose_check = quickCheck(id_compose_f :: (Identity Int) -> Bool)

data Pair a = Pair a a
 
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- need to declare this generator to allow quickcheck
-- know how to generate 'Int' in its tests.
instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = Yeppers <$> arbitrary
possibly_id_f x = (fmap id x) == x
possibly_check = quickCheck (possibly_id_f :: Possibly Int -> Bool)
possibly_compose_f x = functorCompose (\a -> a + 1) (\b -> b + 1) x 
possibly_compose_check = quickCheck (possibly_compose_f :: Possibly Int -> Bool)

newtype Constant a b = Constant { getConstant :: a } deriving (Show) -- 'b' is a phantom type
instance Functor (Constant a) where
  fmap f (Constant a) = Constant a


-- IO Functor
-- we have seen the IO type in the modules and testing chapters already, 
-- but we were not doing much with it save to print text or ask for string
-- input from the user. The IO type will get a full chapter of its own later in the 
-- book. It is an abstract data type; there are not data constructors that oyu are 
-- permitted to pattern-match on, so the typeclasses IO provides are the only 
-- way you can work with value sof type IO a. 
--
-- In case you were wondering how the types are generated, the following
-- illutrates how:
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- read :: Read a => String -> a
-- fmap read :: (Functor f, Read b) => f String -> f b
-- getLine :: IO String
-- fmap read getLine :: (Read b) => IO String -> IO b

