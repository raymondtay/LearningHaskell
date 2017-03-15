{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Control.Applicative (liftA2)

-- This is the compose type. It should look to you much like function
-- composition, but in this case, the f and g represent type constructors, not
-- term-level functions.
--
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- So, we have a type constructor that takes three type argumnets: f and g must
-- be type constructors themselves, while a will be a concrete type.
--
-- Turns out we can get a Functor instance for Compose, too, if we ask that
-- both f and g have Functor instances.
--
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose g) = Compose $ (fmap . fmap) f g

-- Now, the important part here is to appreciate `fmap` because its being
-- applied twice so that we can get the `a` that's embedded inside `f(g a)`
--

-- I discovered, just, that i didn't quite understood what Compose was really
-- about. Nonetheless,here's a simpler example which involves 1 layer
-- and i can imagine that 'f' is some kind of a function which consumes some
-- value of type 'a'. I find it useful to think about what some values might
-- look like :  e.g. One (Just 4), One [4] and so if we want to get to the
-- values hidden within that layer i.e. 4, then we need to fmap our function
-- (assuming its 'g') and apply 'g' to that value. 
--
newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One g) = One $ fmap f g

-- Literally, there're 3 layers. and we apply the same understanding we found
-- the last time round.
--
newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)
instance (Functor f, Functor g , Functor h) => Functor (Three f g h) where
  fmap f (Three g) = Three $ (fmap . fmap . fmap) f g

-- The good thing about 'Compose' is that it allows us to express arbitrarily
-- nested types.
--

-- Let me explain what's going on here....this deserves a lot of explanation
-- lest you might think its MAGIC
-- 
-- When writing the <*> implementation, its important to know how to lift the function
-- a -> b and apply them to the embedded value of type 'a' and hence we use `liftA2` 
-- but how to explain that the lifted function is <*> ???
-- The reason is because <*> consumes `Applicative f => f (a -> b)` and we know in Compose,
-- this means `Compose f g (a -> b)` .

-- DIGEST THIS FOR A WHILE
--



-- *Chapter25> :t liftA2 (<*>)
-- liftA2 (<*>)
-- :: (Applicative f1, Applicative f) =>
-- f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)
--
-- When writing the `pure` implementation, its important to discover how we can lift an ordinary
-- value of type 'a' into `Compose`. The best way is to use the very nature of 'f' and 'g' since thye
-- are already Applicatives. Hence, you see 2 applications of `pure` which represents 'f' and 'g'
-- respectively.
-- *Chapter25> :t pure pure
-- pure pure :: (Applicative f1, Applicative f) => f (a -> f1 a)
--
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure (pure a))
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose g) = 
    Compose (liftA2 (<*>) f g)

