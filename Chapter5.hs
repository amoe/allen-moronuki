module Chapter5 where

x = 42

--addStuff :: Integer -> Integer -> Integer
-- More explicit type signature.
addStuff :: Integer -> (Integer -> Integer)
addStuff a b = a + b + 5

-- Not quite sure what this is intended to prove.

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne = subtractStuff 1


