{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Either(..)) -- hid this explicitly

-- In this case, the f and g represent type constructors, not term-level
-- functions. So, we have a type constructor that takes three type arguments: f
-- and g must be type constructors themselves, whiule a will be a concrete
-- type.
--
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- Running the above, you would see the following :
--
-- *Main> :t getCompose $ Compose [ Just 1, Nothing]
-- getCompose $ Compose [ Just 1, Nothing] :: Num a => [Maybe a]
--
-- *Main> :t getCompose $ Compose [ Just (1 :: Int), Nothing]
-- getCompose $ Compose [ Just (1 :: Int), Nothing] :: [Maybe Int]

-- We need to be able to jump 2 layers into `Compose` to get to the value of
-- type 'a' and then map it over to of type 'b'.
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Here's an example:
-- *Main> fmap (*2) $ Compose [Just 1, Nothing ]
-- Compose {getCompose = [Just 2,Nothing]}

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- This structure is 3-layers deep i.e. the value of type 'a' is hidden
-- 3-layers deep into the "stack".
-- 
-- Looking at these examples so far, the takeaways is to start thinking in a
-- different way i.e. the composition of two datatypes that have a Functor
-- instance gives rise to a new Functor instance. You will sometimes see people
-- refer to this as Functors as being "closed under composition" which just
-- means that when you compose two Functors, you get another Functor.
--
newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

--
-- Applicatives, it turns out, are also closed under composition. We can indeed
-- compose two types that have Applicative instances and get a new Applicative
-- instance. But you are going to write it.
--

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  -- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> Compose fga = Compose ((<*>) <$> f <*> fga)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  -- hint: both f and g are foldables, so this relationship can be leveraged
  -- ... just that it can be non-obvious.
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f1 => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  traverse f (Compose fga) = sequenceA $ fmap f (Compose fga)

-- At this point, if you did wonder why i could perform this `fmap f (Compose f
-- g a)` and the straight forward answer is because both f and g are Functors
-- (by definition of being a Traversable - for both f and g) and if you don't
-- believe , examine and study the function below here "test".
--
tryMe_ :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
tryMe_ f (Compose fga) = fmap f (Compose fga)

-- id :: a -> a means that it would take the input type and return the same.
-- and examine how it is being used here - it is essentially being used to
-- maintain the type coherence.
class Bifunctor p where
  {-#MINIMAL bimap | first , second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- Now, let's write some instances

data Deux a b = Deux a b
data Const a b = Const a 

instance Bifunctor (Const) where
  bimap f g = first f . second g

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g = first f . second g

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g = first f . second g

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g = first f . second g

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g = first f . second g

data Either a b = Left a | Right b

instance Bifunctor (Either) where
  bimap f g = first f . second g

-- About the need for Monad Transformers, the main problem is that we cannot
-- get a new Monad instances by smacking two Monads together. A fundamental
-- problem with composing two Monad lies in the impossibility of joining two
-- unknown Monads. In order to make that join happen, we need to reduce the
-- polymorphism and get concrete information about one ofhte Monads that we are
-- working with. The onther Monad remain polymorphic as a variable type
-- argument to our type constructor.

-- Functions in Haskell, consume arguments from left to right and the symbols
-- involve is likely mind-boggling ... it pays to dive in to look at how the
-- types are constructed (i.e. de-constructed)
--
-- d = (,,) :: a -> b -> c -> (a, b, c)
-- e = ((,,) (<$>)) :: Functor f => b -> c -> (((a -> b1) -> f a -> f b1), b, c)
-- f = ((,,) (<$>) (Just 1) ):: Functor f => c -> ((a -> b1) -> f a -> f b1, Maybe Int, c)
-- g = ((,,) (<$>) (Just 1) (<*>) ):: (Functor f1, Applicative f0) => ((a0 -> b0) -> f1 a0 -> f1 b0, Maybe Integer, f0 (a1 -> b1) -> f0 a1 -> f0 b1)
-- h = ((,,) (<$>) (Just 1) (<*>) (Just "lol")):: Num a => Maybe (c -> (a, [Char], c))
-- ... (etc etc)
--

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative (Identity) where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative f => Applicative (IdentityT f) where
  pure :: a -> IdentityT f a
  pure = IdentityT . pure
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return :: a -> Identity a
  return = Identity
  (Identity a) >>= f = f a

-- Keep in mind that Monad is where we have to really use concrete type
-- information from IdentityT in order to make the types fit.
instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f

-- It may not seem like it, but the IdentityT monad transformer actually
-- captures the essence of transformers generally. We only mebarked on this
-- quest because we could not be guaranteed a Monad instance given the
-- composition of two types. Given that, we know have Functor or Applicative or
-- Monad at our disposal isn't not enough to make that new Monad instance. So
-- what was novel in the following code ? 
--
-- If we examine closely, we might discover that its runIdentityT since we were
-- unable to get the types to "fit" because IdentityT was wedged in the middle
-- of two bits of "m". It turns out to be impossible to fix that using only
-- Functor, Applicative and Monad. This is an exam,ple of why we cannot just
-- make a Monad instance for the Compose type, but we can amake a transformer
-- type like IdentityT where we leverage information specific to the type and
-- combine it with any other type that has a Monad instance. In general, in
-- order to make the types fit.


