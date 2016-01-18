
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday monthNum)
       (Date weekday' monthNum') = weekday == weekday' && monthNum == monthNum'

data Identity a = Identity a

-- the following doesn't quite work because
-- the expression "x == y" means that we need to 
-- constrain the type of 'a' to be 'Eq' 
-- instance Eq (Identity a) where
--   (==) (Identity x) (Identity y) = x == y

instance (Eq a) => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y


instance Show a => Show (Identity a) where
  show (Identity a) = show a

