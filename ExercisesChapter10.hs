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


