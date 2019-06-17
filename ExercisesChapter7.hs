module ExercisesChapter7 where

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

addOne1 x = x + 1
addOne2 = \x -> x + 1

-- Original
addOneIfOdd1 n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

-- Using the anonymous function syntax.
addOneIfOdd2 n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- Original.
addFive1 x y = (if x > y then y else x) + 5

-- Using anonymous function syntax

addFive2 = \x -> \y -> (if x > y then y else x) + 5

-- Original
mflip1 f = \x -> \y -> f y x

mflip2 f x y = f y x
