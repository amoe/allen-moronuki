module EmployeeRank1 where

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ (show e) ++ " is the boss of " ++ (show e')


-- Note that the type here is also IO () because a completely different message
-- has to happen when they are equal rank.  Which actually can't happen, but
-- the compiler can't verify that.
-- I definitely would not have written it this way, but that's the example.
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
  LT -> reportBoss e' e
  EQ -> putStrLn "Neither of them is the boss."
  GT -> (flip reportBoss) e' e 

