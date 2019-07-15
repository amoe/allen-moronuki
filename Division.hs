module Division where

daveDivInternal :: Integer -> Integer -> Integer -> Integer
daveDivInternal n x y  = 
  if r < y then
    n
  else
    daveDivInternal (n + 1) r y
  where r = x - y


daveDiv :: Integer -> Integer -> Integer
daveDiv x y = daveDivInternal 1 x y
