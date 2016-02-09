data Employee = Coder
  | Manager
  | Veep 
  | CEO deriving(Show, Eq, Ord)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = 
  putStrLn $ show e ++ " is the boss of " ++ show e'

-- with the following definition, we can write soemthing like this
-- *Main Data.Tuple> employeeRank (\a b -> compare a b) Veep Coder
-- Veep is the boss of Coder
--
employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO( )
employeeRank f e e' = 
  case f e e' of 
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'


