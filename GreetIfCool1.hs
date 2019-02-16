module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
     then putStrLn "Eyyyyyy.  What's shakin'?"
  else
    putStrLn "Pshhhhh."
  where cool = coolness == "downright frosty yo"
