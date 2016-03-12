
import Data.List (elemIndex) 
import Data.Maybe 

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = Just $ (,) (fromMaybe 1 y) (fromMaybe 1 z)

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = Just $ max' (fromMaybe 1 x') (fromMaybe 1 y')

xs = [1..3]
ys = [4..6]

a :: Maybe Integer
a = lookup 3 $ zip xs ys

b :: Maybe Integer
b = lookup 2 $ zip xs ys

summed :: Maybe [Integer]
summed = Just $ (map $ uncurry (+)) $ maybeToList $ (pure (,) <*> a <*> b)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity 
  (Identity f) <*> (Identity x) = Identity (f x)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

-- pure a :: Applicative f => a -> f a
instance Monoid a => Applicative (Constant a) where
  pure = undefined
  (Constant a) <*> (Constant b) = Constant (mappend a b)

-- 12 March 2016
--
-- instead of writing 
-- const <$> Just "Hello" <*> "World"
-- we could write :
-- const <$> Just "Hello" <*> Just "World"

-- similarly, instead of writing
-- (,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
-- we could write 
-- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]

