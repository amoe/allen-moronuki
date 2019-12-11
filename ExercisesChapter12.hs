module ExercisesChapter12 where

import Data.List (intersperse)

ch12 = "foo"

modifyWord :: String -> String
modifyWord x = case notThe x of
  (Just x) -> x
  Nothing -> "a"


-- Intersperse has been introduced at this stage.
replaceThe :: String -> String
replaceThe x = concat $ intersperse " " $ map modifyWord w
  where w = words x

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

-- 12.5, "String processing", ex2

vowels = "aeiou"

testString = "the evil cow"



isVowelInitialWord :: String -> Bool
isVowelInitialWord w = elem firstLetter vowels
  where firstLetter = head w

-- Fugly and possibly unsafe
pairCount :: [String] -> Integer
pairCount (x:y:xs) = if (isVowelInitialWord y) && x == "the"
                     then 1
                     else 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = countTheBeforeVowelHelper $ words x

countTheBeforeVowelHelper :: [String] -> Integer
countTheBeforeVowelHelper [] = 0
countTheBeforeVowelHelper [x] = 0
countTheBeforeVowelHelper xs = (+) (pairCount thisPair) (countTheBeforeVowelHelper nextPair)
  where thisPair = take 2 xs
        nextPair = drop 1 xs

-- 12.5, "String processing", ex3

-- Cool use of flip here.
countVowels :: String -> Integer
countVowels w = toInteger $ length $ filter ((flip elem) vowels) w

-- 12.5, "Validate the word"

-- Is the fact that it's a newtype actually relevant here?
newtype Word' = Word' String deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel x = elem x vowels

isConsonant :: Char -> Bool
isConsonant x = not $ isVowel x

countPredicate :: (a -> Bool) -> [a] -> Integer
countPredicate f xs = toInteger $ length $ filter f xs

mkWord :: String -> Maybe Word'
mkWord x 
  | nv > nc = Nothing
  | otherwise = Just $ Word' x
  where nv = countPredicate isVowel x
        nc = countPredicate isConsonant x

-- 12.5, "It's only Natural"

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x


