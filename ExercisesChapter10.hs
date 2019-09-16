module ExercisesChapter10 where

import Data.Time

exCh10 = 42


vizFold :: String -> String -> String -> String
vizFold op x y = concat ["(", x, op, y, ")"]

vizFoldFlip :: String -> String -> String -> String
vizFoldFlip op x y = concat ["(", y, op, x, ")"]


foldDemoXs = map show [1..5]

addFoldr = foldr (vizFold "+") "0" foldDemoXs
addFoldl = foldl (vizFold "+") "0" foldDemoXs

mulFoldr = foldr (vizFold "*") "1" foldDemoXs
mulFoldl = foldl (vizFold "*") "1" foldDemoXs

mulFoldrFlip = foldr (vizFoldFlip "*") "1" foldDemoXs
mulFoldlFlip = foldl (vizFoldFlip "*") "1" foldDemoXs


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


fixedFold5a = foldr (++) "" ["woot", "WOOT", "woot"]
fixedFold5b = foldl max 'f' "ear is the little death"

maxCharInString :: [Char] -> Char
maxCharInString [] = undefined
maxCharInString (x:xs) = go xs x 
  where go [] val = val
        go (x:xs) val = go xs (max val x)


fixedFold5c = foldr (&&) True [False, True]

fold5d = foldr (||) True [False, True]



fixedFold5e = foldr ((++) . show) "" [1..5]


-- When using foldl, you can short circuit the rest of the fold.
fixedFold5f = foldl const 'a' [1..5]

-- Even when the argument is a list.
fixedFold5g = foldl const 0 "tacos"

-- You can also do it by using a right fold.
fixedFold5h = foldr (flip const) 0 "burritos"

-- You can also do it by using a right fold.
fixedFold5i = foldr (flip const) 'z' [1..5]


-- Exercises: Database Processing

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Ord, Show)

time1 = UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
time2 = UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)

theDatabase :: [DatabaseItem]
theDatabase = [
  DbDate (UTCTime theDay1 diffSeconds1),
  DbNumber 9001,
  DbString "Hello, world!",
  DbDate (UTCTime theDay2 diffSeconds2)
  ]
  where theDay1 = fromGregorian 1911 5 1
        diffSeconds1 = secondsToDiffTime 34123
        theDay2 = fromGregorian 1921 5 1
        diffSeconds2 = secondsToDiffTime 34123


getTimes :: [DatabaseItem] -> [UTCTime]
getTimes [] = []
getTimes ((DbDate x):xs) = x : (getTimes xs)
getTimes (_:xs) = getTimes xs


getNumbers :: [DatabaseItem] -> [Integer]
getNumbers [] = []
getNumbers ((DbNumber x):xs) = x : (getNumbers xs)
getNumbers (_:xs) = getNumbers xs

-- We need foldr1 for this which hasn't been introduced yet.
-- There's another solution to this but I'm not sure what it is.
mostRecentDate :: [DatabaseItem] -> UTCTime
mostRecentDate xs  = foldr1 max $ getTimes xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (getNumbers xs)


-- use fromIntegral to coerce Integer to double
avgDb :: [DatabaseItem] -> Double
avgDb xs = s / n
  where s = (fromIntegral (sumDb theDatabase)) :: Double
        n = (fromIntegral (length (getNumbers theDatabase))) :: Double







































-- Scan exercises

fibs = 1 : scanl (+) 1 fibs

nthFib :: Int -> Integer
nthFib n = fibs !! n


-- Restrict fibs to the first fibs only.
fibs' = take 20 $ (1 : scanl (+) 1 fibs')

fibs'' = [ x | x <- fibs, x < 100 ]


scanFac :: Integer -> [Integer]
scanFac x = scanl (*) 1 [1..]

-- Somehow you can write factorial as an infinite list using scanl.
facs = scanl (*) 1 [1..]

nthFac :: Int -> Integer
nthFac n = facs !! n
























































-- Chapter exercises...

-- 1 -- generating tuples

stops = "pktdkg"
vowels = "aeiou"

plosives1a :: [Char] -> [Char] -> [(Char, Char, Char)]
plosives1a ss vs = [(s1, v, s2) | s1 <- ss, v <- vs, s2 <- ss]


plosives1b :: [Char] -> [Char] -> [(Char, Char, Char)]
plosives1b ss vs = [(s1, v, s2) | s1 <- ss, v <- vs, s2 <- ss, s1 == 'p']

nouns = ["door", "box", "dog", "house", "car"]
verbs = ["drive", "open", "visit", "hit", "close"]

-- 1c

sentences :: [String] -> [String] -> [(String, String, String)]
sentences ns vs = [(n1, v, n2) | n1 <- ns, v <- vs, n2 <- ns]


-- 2.

seekritFunc :: Fractional a => String -> a
seekritFunc x = (/) (fromIntegral num) (fromIntegral denom)
  where num = sum (map length (words x))
        denom = length (words x)


-- 3.

-- 3a.  Prelude#or


-- Direct recursion, not using (||)
myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) = if x
               then True
               else myOr1 xs


-- Direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs


-- Fold, not point free

-- Notice here the way that "the rest of the fold" corresponds very literally to
-- the recursive call in 'myOr1'.
myOr3 :: [Bool] -> Bool
myOr3 xs = foldr (\a b -> if a then True else b) False xs

myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False
                       

-- 3b.  Prelude#any

-- Direct recursion
myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 f [] = False
myAny1 f (x:xs) = if (f x)
                  then True
                  else myAny1 f xs

-- Direct recursion using (||)
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f [] = False
myAny2 f (x:xs) = (f x) || myAny2 f xs

-- Foldr version, not point free
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f xs = foldr (\a b -> if (f a) then True else b) False xs

-- ???

-- Break it down


foldAny :: (a -> Bool) -> a -> Bool -> Bool
foldAny f a b = if (f a) then True else b

-- Write it like this...
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 f xs = foldr (foldAny f) False xs

-- But  -- composing ?  As we can see that foldAny basically is OR...
-- This is a bit odd, because we're composing with different number of arguments.
-- We can assume that only the first argument gets its bit "composed onto" it.

-- myAny4 even [1,2,3]
-- myAny4 even [1,3,5]

myAny5 :: (a -> Bool) -> [a] -> Bool
myAny5 f = foldr ((||) . f) False

-- I'm sure there's a way to go "more-point-free" than this, but just going to
-- leave it there for the while.

-- 3 -- elem. (member?)

-- Direct recursion
myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 x (y:ys) = if x == y
                   then True
                   else myElem1 x ys

-- Using the or function
myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 _ [] = False
myElem2 x (y:ys) =  x == y || myElem2 x ys


-- Fold with lambda
-- We see again the variable `b` directly represents the right-hand-side of the
-- recursion.
-- Don't forget that this is short-circuiting since the argument 'b' isn't
-- evaluated if the equality-predicate matches.
myElem3 :: (Eq a) => a -> [a] -> Bool
myElem3 x xs = foldr (\a b -> a == x || b) False xs


-- Partially apply the equality function and compose it with or.
myElem4 :: (Eq a) => a -> [a] -> Bool
myElem4 x = foldr ((||) . (== x)) False

-- Using the any function.
myElem5 :: (Eq a) => a -> [a] -> Bool
myElem5 x xs = any (\a -> a == x) xs

-- Pretty point free, I think this is as far as it goes
myElem6 :: (Eq a) => a -> [a] -> Bool
myElem6 x = any (== x)



-- Reverse.

-- Direct recursion
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = (myReverse1 xs) ++ [x]

-- As a fold??
myReverse2 :: [a] -> [a]
myReverse2 xs = foldl (flip (:)) [] xs

-- As a point free fold
myReverse3 :: [a] -> [a]
myReverse3 = foldl (flip (:)) []


-- Map.

-- Direct recursion

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (:) (f x) (myMap f xs)

-- As a foldr.
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldr (\a b -> (:) (f a) b) [] xs

-- Compose cons operator with f.  Maps the first argument, the second is left
-- untouched.
myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr ((:) . f) []


-- Filter.

-- Direct recursion.
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 f [] = []
myFilter1 f (x:xs) = if (f x)
                     then x : (myFilter1 f xs)
                     else (myFilter1 f xs)

-- Foldr with lambda
-- Here you can see how 'b' directly represents "the rest of the computation".
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f xs = foldr (\a b -> if (f a) then a : b else b) [] xs

-- It might be possible to do something with (||) here but can't really figure
-- it out.


-- Squish.

-- Direct recursion.
mySquish1 :: [[a]] -> [a]
mySquish1 [] = []
mySquish1 (x:xs) = (++) x (mySquish1 xs)


-- Foldr?
mySquish2 :: [[a]] -> [a]
mySquish2 xs = foldr (\a b -> (++) a b) [] xs

-- And simpler
mySquish3 :: [[a]] -> [a]
mySquish3 xs = foldr (++) [] xs

-- And even more point free
mySquish4 :: [[a]] -> [a]
mySquish4 = foldr (++) []

-- Squishmap.

-- A function for testing it.
getMultiples :: (Num a) => a -> [a]
getMultiples x = [x * 2, x * 3, x * 4]

-- Direct recursion.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (++) (f x) (squishMap f xs)

-- Foldr/lambda
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f xs = foldr (\a b -> (++) (f a) b) [] xs

-- Foldr pointfree, this works in the same way as the map example.
squishMap3 :: (a -> [b]) -> [a] -> [b]
squishMap3 f xs = foldr ((++) . (f)) [] xs


-- Squishagain.
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- Maximumby.
-- No idea how to do this, I only know the foldl version.
-- I think it's impossible with foldr
-- Need to look at the solution to maximumBy, which is from chapter 9
