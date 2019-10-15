
{-# LANGUAGE PatternSynonyms #-}

module ScratchPad where

--
-- See http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms
--
data Type = App String [Type]

-- This representation is very generic in that no types are given special
-- treatment. However, some functions might need to handle some known types
-- specially, for example the following two functions collect all argument
-- types of (nested) arrow types, and recognize the `Int` type, respectively:
--
collectArgs :: Type -> [Type]
collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
collectArgs _                   = []

isInt :: Type -> Bool
isInt (App "Int" []) = True
isInt _              = False

-- Matching on "App" directly is both hard to read and error prone to write.
-- And the situation is even worse when the matching is nested:
isIntEndo :: Type -> Bool
isIntEndo (App "->" [App "Int" [], App "Int" []]) = True
isIntEndo _                                       = False

-- with "Pattern synonyms" we can do the following
--
pattern Arrow t1 t2 = App "->"    [t1, t2]
pattern Int         = App "Int"   []
pattern Maybe t     = App "Maybe" [t]

collectArgs2 :: Type -> [Type]
collectArgs2 (Arrow t1 t2) = t1 : collectArgs2 t2
collectArgs2 _             = []

isInt2 :: Type -> Bool
isInt2 Int = True
isInt2 _   = False

-- This last example is probably the hardest to understand, take your time to
-- go over it.
isIntEndo2 :: Type -> Bool
isIntEndo2 (Arrow Int Int) = True
isIntEndo2 _               = False

-- The form of pattern synonyms above, ↑ , is bidirectional pattern synonyms.
--

data PosNeg = Pos Int | Neg Int deriving Show
pattern Smarter { nonneg } <- Pos nonneg where -- only positive
  Smarter x = if x >= 0 then (Pos x) else (Neg x)

pattern Smarter' { nonneg' } <- Pos nonneg' where -- only positive
  Smarter' x | x >= 0 = (Pos x)
             | otherwise = (Neg x)

pattern Smarter'' { neg } <- Neg neg where
  Smarter'' x = if x >= 0 then (Pos x) else (Neg x)

isPosNeg :: PosNeg -> Int
isPosNeg (Smarter x) = x
isPosNeg (Smarter' y) = y
isPosNeg (Smarter'' y) = y

test = do
  putStrLn . show $ isPosNeg (Pos 2)
  putStrLn . show $ isPosNeg (Neg 1)
  putStrLn . show $ Smarter   3
  putStrLn . show $ Smarter'  4
  putStrLn . show $ Smarter'' 5
  putStrLn . show $ Smarter   (-1)
  putStrLn . show $ Smarter'  (-4)
  putStrLn . show $ Smarter'' (-3)

pattern HeadC { x } <- x:y:xs where
  HeadC x = [x]

test2 = do
  putStrLn . show $ HeadC ([]::[Int])
  putStrLn . show $ HeadC [1]
  putStrLn . show $ HeadC [1..5]

