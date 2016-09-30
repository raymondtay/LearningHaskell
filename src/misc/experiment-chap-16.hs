module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- making the type argument really explicit
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

liftedReplace'' :: (Functor f, Functor f1) => f(f1 a) -> f(f1 Char)
liftedReplace'' = (fmap . fmap) replaceWithP

liftedReplace''' :: (Functor f2, Functor f1, Functor f) => f(f1 (f2 a)) -> f(f1 (f2 Char))
liftedReplace''' = (fmap . fmap . fmap) replaceWithP
