module EmployeeRank2 where

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ (show e) ++ " is the boss of " ++ (show e')

-- Shadow the real compare
-- Now we can do cool shit like invert the ordering by passing in the flipped
-- comparison function.
employeeRank :: (Employee -> Employee -> Ordering) 
             -> Employee
             -> Employee
             -> IO ()
employeeRank compare e e' = case compare e e' of
  LT -> reportBoss e' e
  EQ -> putStrLn "Neither of them is the boss."
  GT -> (flip reportBoss) e' e 


-- Or use a totally different comparison.
coderSupremacy :: Employee -> Employee -> Ordering
coderSupremacy Coder _ = GT
coderSupremacy _ Coder = LT
coderSupremacy _ _ = EQ
