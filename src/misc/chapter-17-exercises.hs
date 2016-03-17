module Chap17 where

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
-- 
-- the definition of `pure` was a little tricky
-- for me to recognize the pattern, but turns out 
-- that the type constraint of `Monoid a` was the giveaway
-- and it already is a Monoid so it should be as simple as `mempty`
instance Monoid a => Applicative (Constant a) where
  pure a = Constant (mempty a)
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

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
  if (length s) > maxLen 
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

-- This is trickier than most (the reason why is probably because i'm not used to 
-- programming in Haskell as well as i have thought ;) )
-- and i think i should document the thinking process
-- before it gets lost into the infinite space of the Internet
-- Rationale:
-- Applicatives basically take 1 or more functions that's on the LHS 
-- and applies it to every single element on the RHS.
-- what happens from this function application, in our case is that 
-- it would create a list of lists and we need to flatten this double-list
-- structure and we can do that by utilizing the function `append`
-- so you see that i have 
-- append (LHS) (RHS)
-- the (RHS) is the first application of the function on the LHS
-- and we use fmap since `List a` are functors and it produces `List a`
-- the (LHS) is (xs <*> Cons g ys) which forces the compiler to use the
-- applicative again in a recursive manner. 
-- So... what happens from this recursive evaluation is that each application is
-- appended to the previous computation s.t. the results are built up from the 
-- other "end". 
instance Applicative List where
  pure x = Cons x Nil
  (Cons f xs) <*> (Cons g ys) =  append (xs <*> (Cons g ys))  (f <$> (Cons g ys))
  Nil <*> _ = Nil

functions = Cons (+1) (Cons (*2) Nil)
functions' = Cons (*4) Nil
values = Cons 1 (Cons 2 Nil)

append :: List a -> List a -> List a
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as =  concat' $ fmap f as  

-- same behavior as the GHC.List.take function
take' :: Int -> List a -> List a
take' 0 _   = Nil
take' i Nil = Nil
take' i (Cons h t) = Cons h (take' (i - 1) t)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

{-
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l
-}

-- The behavior appears to be the same as GHC.List.repeat
--
repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (ZipList' a) <*> (ZipList' b) =  undefined 

data Validation err a = 
  Failure err | Success a deriving (Eq, Show)

-- One thing to realize is that this is identical to the
-- Either datatype and there is even a pair of total functions which can go 
-- between Validation and Either values interchangeably
--

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a)   = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a

{-
 - Identity laws for `Validation e a`
 -
eitherToValid . validToEither == id
validToEither . eitherToValid == id

-}

data Errors = 
  DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a ) where
  pure x = Second x
  (Second x) <*> (Second y) = x <$> Second(y)

-- the above applicative allows me to write expressions like
-- (Second (+1)) <*> Second 22 => returns me Second 23
--

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)

-- This equation is more interesting than the previous ones i've tried
-- from the perspective that the type `e` is of Monoid but nothing is said
-- of type `a`.
-- Hence, from the equations i've formulated before ... it turns out 
-- there seems like a different strategy when working out which function 
-- to apply when using applicatives. For example, in the case where we detect
-- the presence of Failure types, then we need to use mappend since its already
-- clear from the type signature that e is of type Monoid
-- whereas in the other case, where success types are detected and we see another trend
-- where we would apply <$> instead since the presumption has already been established
-- that all validation types are functors.
instance Monoid e => Applicative (Validation e) where
  pure x = Success x 
  (Success x) <*> (Success y) = x <$> Success(y)
  (Failure e) <*> (Failure f) = Failure (mappend e f)
  (Failure e) <*> (Success _) = Failure e
  (Success _) <*> (Failure e) = Failure e

applyIfBothSecond :: (Sum e) (a -> b) -> (Sum e) a -> (Sum e) b
applyIfBothSecond = undefined

applyMappendError :: Monoid e => (Validation e)(a -> b) -> (Validation e) a -> (Validation e) b
applyMappendError = undefined 

