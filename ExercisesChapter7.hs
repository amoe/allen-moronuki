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


k (x, y) = x

-- has type Integer
k1 = k ((4-1), 10)

-- k2 has type string.
k2 = k ("three", (1 + 2))

-- has type Integer
k3 = k (3, True)



f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, x', x'') (y, y', y'') = ((x, y), (x'', y''))






-- functionC, aka max
functionC :: (Ord a) => a -> a -> a
functionC x y =
  case x > y of
    True -> x
    False -> y


-- Need the Integral type class constraint in order to use the modulus
-- operation.
-- Also, why do we not need the Eq typeclass to be explicitly added to the
-- constraint?
-- The answer is:
-- Integral => Real => Ord => Eq
-- So Integral transitively implies Eq.
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 x = 
  case mod x 2 of
    0 -> x + 2
    _ -> x


nums :: (Num a, Ord a) => a -> Integer
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy $  2


-- Guard Duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100

avgGrade2 :: (Fractional a, Ord a) => a -> Char
avgGrade2 x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: (Eq a) => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Ord a, Num a) => a -> Integer
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1


-- Chapter exercises

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xlast = x `div` 10
        d = xlast `mod` 10
