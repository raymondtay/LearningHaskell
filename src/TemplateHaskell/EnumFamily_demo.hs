{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Proxy
import EnumFamily -- see EnumFamily.hs
import GHC.TypeLits hiding (Mod)

type family Mod (m :: Nat) (n :: Nat) :: Nat
type family Add (m :: Nat) (n :: Nat) :: Nat
type family Pow (m :: Nat) (n :: Nat) :: Nat

enumFamily mod ''Mod 10
enumFamily (+) ''Mod 10
enumFamily (^) ''Mod 10

a :: Integer
a = natVal (Proxy :: Proxy (Mod 6 4))

b :: Integer 
b = natVal (Proxy :: Proxy (Pow 3 (Mod 6 4)))

main :: IO ()
main = undefined

