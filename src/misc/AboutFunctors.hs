module AboutFunctors where


replaceWithP :: a -> Char
replaceWithP = const 'p'

-- lift the function replaceWithP into a higher-order function
twiceLifted :: (Functor f, Functor f1) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- lift it again !
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- lift it 3x 
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- lift it once again !
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted


data X a = X a deriving (Read, Show)
let
  f = read "[2]" :: [X Int]
in
  case f of 
    ((X i):xs) -> do
      print "I got a value: " ++ i
      return i

