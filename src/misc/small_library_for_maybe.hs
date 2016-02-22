module SimpleChecks where


isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _)  = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a ) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just _) = a 

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = [] ++ catMaybes xs
catMaybes ((Just a):xs) = a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe as@(x:xs) = sequence as

