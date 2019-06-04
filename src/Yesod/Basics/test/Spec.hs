{-# LANGUAGE FlexibleContexts #-}

import Data.Monoid hiding ((<>))
import Test.QuickCheck
import Lib

-- Superfluous test defn aka "property test"
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc r1 r2 r3 = (r1 <> (r2 <> r3)) == ((r1 <> r2) <> r3)

-- Superfluous property generator with a frequency generator
instance Arbitrary MyRoute where
  arbitrary =
    frequency [(1, return SpecialRoute),
               (1, return Home),
               (1, return Time),
               (1, return Stylesheet)]

type MyRouteAppend = MyRoute -> MyRoute -> MyRoute -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: MyRouteAppend)
  quickCheck (monoidAssoc :: MyRouteAppend)

