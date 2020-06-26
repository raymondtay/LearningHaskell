{-# LANGUAGE PolyKinds #-}

import Data.Proxy

-- Kind polymorphism 
--
-- type families gives us functions at the type-level (through type functions)
-- as what we have seen in the source files:
-- * TypeSynonymFamilies.hs
-- * AssociatedTypeSynonym.hs
--
-- Kind polymorphism gives us the ability to describe more generic data and
-- functions. For example, when designing a type-class, the need may arise to
-- cater for various kind-orders.
-- 
-- Without PolyKinds enabled, the instance definition for T0 would require us
-- to complete the type signature to satisfy the "kind" requirement of T0 which
-- 

class T0 a where
  f0 :: a -> String

instance T0 Int where
  f0 _ = "T0 Int"
instance T0 Char where
  f0 _ = "T0 Char"
instance T0 (Maybe a) where -- its necessary to fill so that the kind is * instead of * -> *
  f0 _ = "T0 Maybe"

-- We need a higher-kinded typeclass to deal with this and turns out the
-- "higher-kindedness" is not represented by the "m" at the type-class
-- definition level but actually at "f1" 
--
class T1 m where
  f1 :: Show a => m a -> String
  f2 :: m a -> String

instance T1 Maybe where
  f1 _ = "T1 Maybe"
  f2 _ = "What?"

-- This declaration allows only Maybe values i.e. Just X or Nothing
--
--
-- With the Polykinds extension, 'k' would be polymorphic by default.
-- Conversely, without the Polykinds extension, the compiler would be
-- complaining that "Maybe needs an argument to make sure the kind is *"
-- 
--
class T a where -- (a :: k )
  f :: Proxy a -> String

instance T Maybe where -- don't forget that Maybe is Functor, Applicative and Monad so this extension encompasses that as well.
  f _ = "T Maybe"

instance T Int where
  f _ = "T Int"

-- The particular type of Proxy we pass to "f" determines the type-class at
-- which the function will be invoked. In this example, kind-polymorphism
-- appears in different guises:
-- * T is a kind-polymorphic type-class
-- * The Proxy datatype is kind-polymorphic (it takes types of any kind-order
--   and returns a * kind)
-- * The Proxy data-constructor is a kind-polymorphic function
--


