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

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- I know that whenever i see something like LiftItOut (f a) then 'f' is
-- actually a function and together with (f a) it means that its function
-- application. therefore, we now can actually lift out this function
-- application 'fa' via fmapping it but the requirement is that the function
-- that's embedded in 'fa' must actually be a Functor .... which explains the
-- final signature.
--
data LiftItOut f a = LiftItOut (f a) deriving Show
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- To understand the following code snippet, i again apply the reasoning i used
-- before in the last code for `LiftItOut` and the same rationale applies.
--
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


-- To understand this code snippet, the main thing is to realize that i can
-- safely ignore fmapping over 'fa' because its actually part of the functor
-- definition and we should focus on the 'b' since that's going to be subjected
-- to fmapping and its actually embedded in 'gb' (because we know 'gb' eqv. (g b))
-- and to fmap over 'gb' means that g ∈ Functor.
--
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) 
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)



-- Again, the following example demonstrates how we should always keep our
-- focus on the type parameter that is not bounded to the functor definition
-- and from there, we know (almost by instinct) that i need to fmap over 'gt'.
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- This is slightly trickier than the previous ones. The tricky part is that
-- the data constructor "seems" to be the same as the type constructor. In
-- Haskell, we need to remember that they are two different things though they
-- look exactly the same.
-- 
-- In here, i used 't' to represent (List a) since it literally is the tail of
-- the list. Next, i fmap over 't' and i do not need to place a type constraint
-- in the declaration of the functor because its already there. i.e. List ∈
-- Functor.
data List a = Nil | Cons a (List a) 
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)


-- This should be easier to understand now - if you had understood the previous
-- examples.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a) 
  fmap f (MoreGoats aa bb cc) = MoreGoats (fmap f aa) (fmap f bb) (fmap f cc)

-- 
-- Prelude defines type String = [Char]
-- 
-- data TalkToMe a = Halt | Print String a | Read (String -> a)
-- 
data TalkToMe a = Halt | Print [Char] a | Read ([Char] -> a)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (f . sa)

