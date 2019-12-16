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

nonNegativeIntegerToNat :: Integer -> Nat
nonNegativeIntegerToNat 0 = Zero
nonNegativeIntegerToNat x = Succ (nonNegativeIntegerToNat (x - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (nonNegativeIntegerToNat (x - 1))

-- roundtrips successfully
roundTrip = case (integerToNat 10) of
              (Just x) -> natToInteger x
              Nothing -> error "foo"            
                 
  
-- 12.5, "Small library for Maybe"

-- Simple boolean checks for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- The 'Maybe catamorphism'

-- In the call: mayybee 0 (+1) (Just 1)
-- b is bound to 0, type Num a => a
-- the second arg is a function from a -> b
-- concretely, Num a => a -> a
-- intuitively this will just unwrap the Maybe and apply it.
-- In the case of the Nothing, you don't even ever apply the function.
-- It's impossible for you to apply it, because z is of type b.
-- So just return b directly.
-- So this is basically quite similar to a function that I used to want in clojure
-- that encapsulates the (if (predicate) (f x) x) pattern.

-- Practically, it lets you provide a fallback for the nothing case, and
-- transform the contained value in the Just case.  You could pass 'id' to return
-- the unwrapped value directly.  But the z fallback-value must type-match the
-- type of the result function, so you always get "one type" at the call site.

-- It's sort of like a combination of orElse() and flatMap().

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f Nothing = z
mayybee z f (Just x) = f x

-- fromMaybe, a simplified version with no transform step

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

-- When it's written in terms of the previous one:

-- java is Optional.orElse()
fromMaybe' :: a -> Maybe a -> a
fromMaybe' x y = mayybee x id y


-- listToMaybe

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x
