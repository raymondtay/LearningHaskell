{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Plus where

-- if i were to simply load this file with the duplicated instance declaration
-- commented out as shown here, then GHC 8.4.3 would not be showing me
-- compilation errors.
--
class Plus a b c | a b -> c where
  plus :: a -> b -> c

instance Plus Int Int Int where plus = (+)

instance Plus Int Float Int where plus x y = (+) x (round y)

-- instance Plus Int Float Int where plus x y = (+) x (round y)

