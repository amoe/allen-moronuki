{-# LANGUAGE NoMonomorphismRestriction #-}
module ExercisesChapter6 where

import Data.List (sort)

x = 42

-- Does not type check -- no Show instance on Person.
-- data Person = Person Bool
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)

-- Does not type check -- no Eq instance on Mood.
-- data Mood = Blah | Woot deriving Show
-- settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Won't type check.  Need explicit type coercion.
--phew = Papu "chases" True

-- Type checks and yields a partially-applied data constructor.
truth = Papu (Rocks "chomskydoz")

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Need Ord.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

i :: Num a => a
i = 1

-- f :: Num a => a
-- f = 1.0

f2 :: Fractional a => a
f2 = 1.0

f3 :: RealFrac a => a
f3 = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- Will not work because return type would be bound by argument type.
-- sigmund :: a -> a
-- sigmund x = myX

sigmund' :: Int -> Int
sigmund' x = myX

-- Will not work because myX is already a more specific type, Int.
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jung2 :: [Int] -> Int
jung2 xs = head (sort xs)

young :: [Char] -> Char
young xs = head (sort xs)

young2 :: Ord a => [a] -> a
young2 xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Will not work.  List of Ords cannot be passed to mySort.
-- signifier2 :: Ord a => [a] -> a
-- signifier2 xs = head (mySort xs)

