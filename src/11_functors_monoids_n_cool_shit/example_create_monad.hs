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
    return x = Probability [(x,1)]
    (Probability xs) >>= f = join (fmap f (Probability xs))

