module StartWithMonadT where

newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

--
-- We need to apply `fmap` twice to get to the 
-- value `inside`. Why? The answer is to work through 
-- the types ... and here's how:
-- when i say `fmap f` i'm applying "f" to the functor 
-- and this works when i say `fmap (+1) (Just 4)` but when
-- (Just 4) is embedded in another functor? e.g. [Just 4] the question
-- is how do we get to the Maybe inside []????
-- Answer is to apply fmap twice. i.e. `(fmap . fmap $ (+1)) [Just 4]`
-- and you get the answer `[Just 5]` and you would noticed that the outer-structure
-- [] wasn't "touched".
--
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

instance Functor f => Applicative (One f) where
  pure a = One $ id a
  (<*>)  = undefined

newtype Three f g h a = 
  Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

