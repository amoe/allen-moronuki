module GreetIfCool2 where

-- Note that haskell has special sugar allowing you to use where-clause syntax
-- to directly define functions.

-- Here, 'cool' is a function that is called to determine the coolness of the
-- string.

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
     then putStrLn "Eyyyy.  What's shakin'?"
  else
    putStrLn "Pshhhhhh."
  where cool v = v == "downright frosty yo"
