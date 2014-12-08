{-# LANGUAGE NoMonomorphismRestriction #-}

-- The haskell-98 standard ahs a subtle feature that can sometimes bite us in 
-- unexpected circumstances. And one thing to prevent this is to use either
-- the compiler pragma (as i've chosen to do here since its convenient instead of remembering
-- to pass the right compiler option i.e. -fno-monomorphism-restriction 

myShow = show

-- however, without the pragma/ compiler option it would fail to load and it is instructive to learn
-- the reason why. The monomorphism restriciton to which the error message refers to is a part of the
-- Haskell 98 stnadard. Monomorphism is simply the opposite of polymorphism: it indicates that 
-- an expression has exactly one type. The restriction lies int he fact that Haskell forces a declaration
-- to be less polymorphic that we would expect.

