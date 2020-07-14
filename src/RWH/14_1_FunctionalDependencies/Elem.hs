{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Using the functional notation, 'Elem c' is a way of writing the element type
-- of the collection 'c'.
-- class Elem c e | c -> e where
--   insert :: Elem c -> c -> c
--   toList :: c -> [Elem c]

class RiemannHypothesis where
  assumeRH :: a -> a

instance RiemannHypothesis where
  assumeRH = id

--isPrime :: RiemannHypothesis => Integer -> Bool
--isPrime n = assumeRH (...)
class Coll e c | c -> e where
  empty :: c
  insert :: e -> c -> c

