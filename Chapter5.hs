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


davesAddition :: Num a => a -> a -> a
davesAddition x y = x + y

-- This type signature
davesAddition2 :: (Num a, Num b) => a -> b -> b
davesAddition2 x y = x + y
