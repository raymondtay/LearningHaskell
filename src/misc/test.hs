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

