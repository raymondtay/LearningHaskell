-- 
-- Typeclass inheritance is when a typeclass has a superclass. 
-- This is a way of expressing that a typeclass requires another typeclass
-- to be available for a given type before you can write an instance.
--
-- class Num a => Fractional a where
--  (/) :: a -> a -> a
--  recip :: a -> a
--  fromRational :: Rational -> e
--
--  Here, the typeclass Fractional inherits from Num. We could say 
--  that Num is a superclass of Fractional . The long and short
--  of it is that if you want to write an instance of Fractional for 
--  some "a", that type "a", must already have an instance of Num before you 
--  may do so.
--

newtype Nada = Nada Double deriving(Eq, Show)

instance Fractional Nada where
  (Nada x) / (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)

-- for this to work, Nada must be a Num !
-- and we basically need to provide function definitions
-- for (+), (-), (*), negate, abs, signum, fromInteger
-- so that Nada can behave like a Num.
-- The code below shows how this can typecheck and there's q
-- quite a fair bit of mucking around number conversions
--

instance Num Nada where
  (+) (Nada a) (Nada b) = Nada (fromIntegral $ ceiling(a) + ceiling(b))
  (-) (Nada a) (Nada b) = Nada (fromIntegral $ ceiling(a) - ceiling(b))
  (*) (Nada a) (Nada b) = Nada (fromIntegral $ ceiling(a) * ceiling(b))
  negate (Nada a) = Nada (-a)
  abs (Nada a) = Nada (fromIntegral $ ceiling(a))
  signum (Nada a) = Nada (1)
  fromInteger i = Nada ((fromIntegral i) :: Double)
