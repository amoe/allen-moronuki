module Chapter7 where

myNum :: Integer
myNum = 1

myVal f = myNum

myVal2 f = f + myNum

-- The type that ghc infers for this is:
-- t -> t1 -> Integer
-- Note that the two types can potentially differ.
myVal3 f g = myNum

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was " ++ show x ++ " and y was " ++ show y

bindExp2 :: Integer -> String
bindExp2 x = 
  let x = 10
      y = 5 in
    "the integer was " ++ show x ++ " and y was " ++ show y

triple = \x -> x * 3


