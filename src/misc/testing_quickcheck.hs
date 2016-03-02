import Control.Monad
import Data.Monoid
import Test.QuickCheck
import MyOptional 

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity a = (a <> mempty) == a
monoidRightIdentity a = (mempty <> a) == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Show)
instance Monoid (First' a ) where
  mempty = First' Nada
  mappend (First' (Only a)) (First' Nada) = First' (Only a)
  mappend (First' Nada) (First' (Only a)) = First' (Only a)
  
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency[ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where 
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

