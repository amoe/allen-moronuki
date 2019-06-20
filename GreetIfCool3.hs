module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool x =
  case isCool of
    True -> putStrLn "Some thing c"
    False -> putStrLn "Some non thing"
  where isCool = x == "mellon"
    
