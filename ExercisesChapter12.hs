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
