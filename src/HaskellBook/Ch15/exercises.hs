{-# LANGUAGE InstanceSigs #-}
module Chapter15 where

data Optional a = Nada | Only a deriving (Eq, Show)

-- 
-- Definition of monoid for Optional
--
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend :: Optional a -> Optional a -> Optional a
  mappend Nada _ = Nada
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

