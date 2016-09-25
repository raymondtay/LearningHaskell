module Chapter6 where

-- This is probably one of the easiest examples
-- to understand typeclasses in haskell. 
--
class Numberish a where
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer
  defaultNumber :: a

-- `newtype` only allows value constructor that takes in 1 type
newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber x = Age x
  toNumber (Age x) = x
  defaultNumber = Age 1 -- must be toddler

sumOfAges :: Numberish a => a -> a -> a
sumOfAges a a' = fromNumber $ (+) (toNumber a) (toNumber a')

-- type constructors participate in type expressions
-- data constructors participate in value-expressions
data DayOfWeek = Mon | Tues 
data Date = Date' DayOfWeek Int
instance Eq DayOfWeek where
 (==) Mon Mon = True
 (==) Tues Tues = True
 (==) _ _ = False
instance Eq Date where
 (==) (Date' wk wm) (Date' wk' wm') = wk == wk' && wm == wm'

data Identity a = Identity' a 

-- instance Eq (Identity t) where
--   (==) (Identity' a) (Identity' a') = (==) a a'
-- the above declaration would fail compilation
-- because haskell does not know anything about a and a'
-- so we need to tell haskell that t must also be an Eq
-- like the following - which compiles perfectly.
--
instance Eq t => Eq (Identity t) where
  (==) (Identity' a) (Identity' a') = (==) a a'

-- at this point in time, we can test for equality.
--
--
--
-- Now, let's do some exercises ...

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson p = putStrLn(show p)

data Mood = Blah | Woot deriving Show

instance Ord Mood where
  compare a a' = if a == a' then EQ else LT

instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _ = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Show, Ord)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

coderRules :: Employee -> Employee -> Ordering
coderRules Coder Coder = EQ
coderRules Coder _     = GT
coderRules _     Coder = LT
coderRules e e'        = compare e e'


employeeRank :: (Employee -> Employee -> Ordering)
            -> Employee -> Employee -> IO()
employeeRank f e e' = 
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n -1 ) f $ b
