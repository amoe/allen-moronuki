module ExercisesChapter9 where

import Data.Bool (bool)
import Data.Char (isUpper, toUpper, ord, chr)
import Data.List (maximumBy)


-- Ex: Comprehend thy lists

mySqr = [ x^2 | x <- [1..10] ]

lcVal1 = [ x | x <- mySqr, rem x 2 == 0 ]


wanted2 = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81), (4, 100), (9, 64), (9, 81), (9, 100), (16, 64), (16, 81), (16, 100), (25, 64), (25, 81), (25, 100), (36, 64), (36, 81), (36, 100), (49, 64), (49, 81), (49, 100)]

lcVal2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

wanted3 = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]

lcVal3 = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- Ex: Square cube

mySqr2 = [ x^2 | x <- [1..5] ]

myCube2 = [ y^3 | y <- [1..5] ]

someTuples1 = [ (x, y) | x <- mySqr2, y <- myCube2 ]

-- length someTuples2 = 15
someTuples2 = [ (x, y) | x <- mySqr2, y <- myCube2, x < 50, y < 50 ]


--- Ex: Bottom madness

expr1 = [ x^y | x <- [1..5], y <- [2, undefined] ]

expr2 = take 1 expr1

expr3 = sum [1, undefined, 3]

expr4 = length [1, 2, undefined]

expr5 = length $ [1, 2, 3] ++ undefined

expr6 = take 1 $ filter even [1, 2, 3, undefined]

expr7 = take 1 $ filter even [1, 3, undefined]

expr8 = take 1 $ filter odd [1, 3, undefined]

expr9 = take 2 $ filter odd [1, 3, undefined]

expr10 = take 3 $ filter odd [1, 3, undefined]



-- Ex: More bottoms

-- 4.  What is its type?
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5.
mapval1 = map (^2) [1..10]

mapval2 = map minimum [[1..10], [10..20], [20..30]]

mapval3 = map sum [[1..5], [1..5], [1..5]]


ex6 = map (\x -> bool x (negate x) (x == 3))  [1..5]

-- Ex: Filtering

filterAnswer1 = filter (\x -> (rem x 3) == 0) [1..30]

multiplesOfThree = filter (\x -> (rem x 3) == 0)

howMany3X = length . multiplesOfThree

articles = ["the", "a", "an"]

myFilter :: String -> [String]
myFilter x = filter (\x -> not (isArticle x)) tokens
  where tokens = words x
        isArticle x = elem x articles


-- This is my hand written version of zip.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (:) (x, y) (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = (:) (f x y) (myZipWith f xs ys)

-- Now we can define zip in terms of zipWith.
myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (,)

-- Using point free notation
retainUppercase :: [Char] -> [Char]
retainUppercase = filter isUpper


capitalizeFirstLetter :: [Char] -> [Char]
capitalizeFirstLetter (x:xs) = (:) (toUpper x) xs


-- Of course, this is basically just a map.
shout :: [Char] -> [Char]
shout [] = []
shout (x:xs) = (:) (toUpper x) (shout xs)

getUpperFirstElement :: String -> Char
getUpperFirstElement x = toUpper $ head x


getUpperFirstElementComposed :: String -> Char
getUpperFirstElementComposed x = (toUpper . head) x

getUpperFirstElementPointfree :: String -> Char
getUpperFirstElementPointfree = toUpper . head


shiftUp :: Enum a => Integer -> a -> a
shiftUp 0 y = y
shiftUp x y = shiftUp (x - 1) (succ y)

-- This caesar works but has the problem that it doesn't wrap around properly.
basicCaesar :: [Char] -> [Char]
basicCaesar x = map (shiftUp 5) x

variableCaesar :: Integer -> [Char] -> [Char]
variableCaesar x y = map (shiftUp x) y


-- we want to start off at the base, 97 (the value of ord 'a')
-- let's say we are requested to shift 'b'
-- ord 'b' is 98
-- 98 + 5 = 103

-- if we are requested to mod x by 5, the answer should be 'c' == 99
-- naive way would give 120 + 5 == 125

-- By doing 120 - ord 'a', we know that the relative position within the alphabet
-- is 23.
-- Now we do 23 + 5 = 28
-- Now we do mod 28 26 == 3


shiftUpModular :: Int -> Char -> Char
shiftUpModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'a'
        relativePosition = (ord x) - base
        newPosition = relativePosition + n

-- The change here is in the newPosition calculation.
shiftDownModular :: Int -> Char -> Char
shiftDownModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'a'
        relativePosition = (ord x) - base
        newPosition = relativePosition - n


modularCaesar :: Int -> [Char] -> [Char]
modularCaesar n xs = map (shiftUpModular n) xs        

modularUncaesar :: Int -> [Char] -> [Char]
modularUncaesar n xs = map (shiftDownModular n) xs        



-- Writing your own version of Prelude functions...

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False:_) = False
myAnd (_:xs) = myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (_:xs) = myOr xs

-- A cheaty way
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = or $ map f xs

-- Writing myElem recursively
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys
  

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 x ys = any (== x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- squish is actually concat.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = (++) x (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs


squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- Introduce an explicit counter to prevent trying to evaluate (f x undefined)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f xs undefined 0
  where go f [] last _ = last
        go f (x:xs) _ 0 = go f xs x 1
        go f (x:xs) last n = case f x last of
                               LT -> go f xs last (n + 1)
                               EQ -> go f xs last (n + 1)
                               GT -> go f xs x (n + 1)


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = go f xs undefined 0
  where go f [] last _ = last
        go f (x:xs) _ 0 = go f xs x 1
        go f (x:xs) last n = case f x last of
                               LT -> go f xs x (n + 1)
                               EQ -> go f xs last (n + 1)
                               GT -> go f xs last (n + 1)
  

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
