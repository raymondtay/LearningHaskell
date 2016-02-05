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

