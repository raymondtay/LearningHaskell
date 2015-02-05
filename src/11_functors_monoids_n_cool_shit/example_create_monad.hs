import Data.List (all)
import Data.Ratio
import Control.Monad

newtype Probability a = Probability { getProbability :: [(a, Rational)] } deriving Show

instance Functor Probability where
    fmap f (Probability xs) = Probability $ map (\(x,p) -> (f x, p)) xs

{-
 with the above expression, we can now write the following 
 *Main> fmap negate (Probability [(3, 1%4), (5, 1%2), (9, 1%4)])
 Probability {getProbability = [(-3,1 % 4),(-5,1 % 2),(-9,1 % 4)]}
 and u'll notice that the functor instance we've define does exactly what we
 defined it to do.
-}

instance Monad Probability where
    return x = Probability [(x,1%1)]
    (Probability xs) >>= f = join (fmap f (Probability xs))
    -- (Probability xs) >>= f = flatten (fmap f (Probability xs)) -- works the same !
    fail _ = Probability []

thisSituation :: Probability (Probability Char)
thisSituation = Probability [( Probability [('a',1%2),('b',1%2)], 1%4 ),
                             ( Probability [('c',1%2),('d',1%2)], 3%4 )]


flatten :: Probability (Probability a) -> Probability a
flatten (Probability xs) = Probability $ concat $ map multAll xs
    where multAll (Probability innerxs, p) = map (\(x,r) -> (x, p*r)) innerxs

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Probability Coin 
coin = Probability [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Probability Coin 
loadedCoin = Probability [(Heads, 1%10), (Tails, 9%10)]


flipThree :: Probability Bool
flipThree = do
    a <- coin
    b <- coin 
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

