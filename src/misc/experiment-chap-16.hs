module ReplaceExperiment where

-- 
-- Lifting in functional programming is to 
-- really get to the value hidden within contexts.
-- E.g. when we have a value that's "embedded" inside
-- several layers of contexts and we want to run functions
-- to transform the value, then we need a elegant way to 
-- get to it and lifting is one way.
--
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

thriceLifted :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted = liftedReplace'''

main :: IO ()
main = do
  print (replaceWithP' lms)
  print (liftedReplace lms)
  print (liftedReplace' lms)
  print (liftedReplace'' lms)
  print (liftedReplace''' lms)

