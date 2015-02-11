-- Given the following defns, ...

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f,g) x = (f x, g x)

unzip = fork (map fst, map snd)
cross (f, g) = fork( f . fst, g . snd )

-- an example of using `cross` would be :
-- map (cross (\x -> x + 1, \x -> x * 2)) (zip [1..10] [1..10])
-- gives [(2,2),(3,4),(4,6),(5,8),(6,10),(7,12),(8,14),(9,16),(10,18),(11,20)]

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

type Pair a b = (a, b)

-- expressing `cross` interms of `bimap` for the instance `Pair` of `Bifunctor`
instance Bifunctor (Num x, Num y) => (Pair x y) where
    bimap f g (a, b) = (f a, g b) :: Pair a b 

instance Bifunctor Either where
    bimap f g (Left x) = Left (f x )
    bimap f g (Right x) = Right (g x )
    
