data DayOfWeek = Mon| Tue | Wed | Thur | Fri | Sat | Sun
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thur Thur = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = True

instance Eq Date where
  (==)  (Date weekday numberOfMonth) 
        (Date weekday' numberOfMonth') = 
    weekday == weekday' && numberOfMonth == numberOfMonth'

data Identity a = Identity a

-- Allows us to compare 
-- Identity 1 == Identity 1
instance (Eq a) => Eq (Identity a) where
  (==) (Identity a) (Identity b) = a == b

-- Allows us to make comparisons like
-- Identity 1 < Identity 2
-- Identity 2 >= Identity 2
instance Ord a => Ord (Identity a) where
  compare (Identity a) (Identity b) = compare a b 
