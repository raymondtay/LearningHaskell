{-# LANGUAGE FlexibleContexts #-}

module SmallLibraryForEither where

{- 
 - Most of the functions you just saw are in the Prelude, Data.Maybe 
 - or Data.Either but you should strive to write them yourself without 
 - looking at existing implementations. You will deprive yourself 
 - if you cheat.
 -
 -}
-- Try to eventually arrive at a solution that uses foldr,
-- even if earlier versions dont use foldr.

lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\e -> \acc -> case e of (Left f) -> f:acc; otherwise -> acc) [] xs

-- Same as the last one. Use foldr eventually.
--
rights' :: [Either a b] -> [b]
rights' xs = foldr (\e -> \acc -> case e of (Right f) -> f:acc; otherwise -> acc) [] xs

partitionEithers' :: [Either a b] ->([a], [b])
partitionEithers' xs = (foldr (\e -> \acc -> case e of (Left f) -> f:acc; otherwise -> acc) [] xs,
                        foldr (\e -> \acc -> case e of (Right f) -> f:acc; otherwise -> acc) [] xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right a) = Just $ f a
eitherMaybe' f _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left  a) = f a
either' f g (Right a) = g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left a)  = eitherMaybe' f (Left a)
eitherMaybe'' f (Right a) = eitherMaybe' f (Right a)



