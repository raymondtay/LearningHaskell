{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind

data Mood = Blah | Woot deriving Show

data JMood = Just Mood deriving Show

{-
 - what we can do is to type this on the ghci interpreter
 - changeMood $ changeMood Blah
 - $ Blah
 -}

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# xs) = 1 + hLength xs


applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5


