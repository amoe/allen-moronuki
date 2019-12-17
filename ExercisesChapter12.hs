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
  | otherwise = Just (nonNegativeIntegerToNat x)

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


-- listToMaybe pulls out the first element or nothing.   It's sort of referencing


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- catMaybes -- cats maybes and drops nothing


-- catMaybes :: [Maybe a] -> [a]
-- catMaybes [] = []
-- catMaybes (Nothing:xs) = catMaybes xs
-- catMaybes ((Just x):xs) = x : catMaybes xs


-- A function that will be folded with foldr, (a -> b -> b)
possiblyCons :: Maybe a -> [a] -> [a]
possiblyCons Nothing ys = ys
possiblyCons (Just x) ys = (:) x ys

-- or alternately can it be written as a fold?  Remember that we have to both
-- destructure and filter.
-- I don't think that you can have pattern matching on a lambda.
catMaybe :: [Maybe a] -> [a]
catMaybe xs = foldr possiblyCons [] xs

-- This demonstrates the alternate way to do it using case.
-- We could also write 'integerToNat' in this way, probably.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe ((Just x):xs) = case (flipMaybe xs) of
                            Nothing -> Nothing
                            (Just y) -> Just (x:y)


-- go from a list of eithers and locate all of the Justs
lefts :: [Either a b] -> [a]
lefts [] = []
lefts ((Right _):xs) = lefts xs
lefts ((Left x):xs) = x : lefts xs

foldFn1 :: Either a b -> [a] -> [a]
foldFn1 x xs = case x of
  (Left y) -> y : xs
  (Right y) -> xs

lefts' :: [Either a b] -> [a]
lefts' xs = foldr foldFn1 [] xs

-- Even more compact

lefts'' :: [Either a b] -> [a]
lefts'' xs = foldr f [] xs
  where f x xs = case x of
          (Left y) -> y : xs
          (Right y) -> xs

-- Yet even more compact
lefts''' :: [Either a b] -> [a]
lefts''' = foldr f []
  where f (Left x) xs = x:xs
        f (Right _) xs = xs


-- Same approach
rights :: [Either a b] -> [b]
rights = foldr f []
  where f (Left _) xs = xs
        f (Right x) xs = x:xs


-- The simple recursive one
-- note that we repeat the where clause -- we could avoid this by using a case
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers ((Left x):xs) = (x:ls, rs)
  where (ls, rs) = partitionEithers xs
partitionEithers ((Right x):xs) = (ls, x:rs)
  where (ls, rs) = partitionEithers xs


-- Yeah nice ...
-- Destructure the rest-of-the-computation (z) into ls and rs,
-- then just case on the result.
foldFn2 :: Either a b -> ([a], [b]) -> ([a], [b])
foldFn2 x z = case x of
                (Left y) -> (y:ls, rs)
                (Right y) -> (ls, y:rs)
  where (ls, rs) = z

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr foldFn2 ([], []) xs


-- Here's an innovation, destructure inside the function pattern, obviating
-- the need to nest the where clause.
-- Think that this is the most compact version.
partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr f ([], [])
  where f x (ls, rs) = case x of
                         (Left y) -> (y:ls, rs)
                         (Right y) -> (ls, y:rs)

