module Chapter12 where

import Data.List (foldl', unfoldr)

ch12 = "foo"

data Maybe' a = Nothing' | Just' a

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 x = 
  if even x
  then x + 2
  else error "not even"


ifEvenAdd2' :: Integer -> Maybe Integer
ifEvenAdd2' x = 
  if even x
  then Just (x + 2)
  else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- A wrapper function that does the required validation
--  This is called a 'smart constructor'.
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age 
  | name == ""  = Nothing
  | age < 0 = Nothing
  | otherwise = Just (Person name age)

data Either' a b = Left' a | Right' b

data PersonInvalid = NameEmpty | NegativeAge deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age 
  | name == ""  = Left NameEmpty
  | age < 0 = Left NegativeAge
  | otherwise = Right (Person name age)

-- Now the combined validation functions.

-- Either a list of PersonInvalid possibilties, or a real Age.
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay x =
  if x >= 0
  then Right x
  else Left [NegativeAge]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay x =
  if x /= ""
  then Right x
  else Left [NameEmpty]

mkPerson'' :: Name -> Age -> Either [PersonInvalid] Person
mkPerson'' x y = mkPerson''' (nameOkay x) (ageOkay y)

mkPerson''' :: Either [PersonInvalid] Name -> 
               Either [PersonInvalid] Age -> 
               Either [PersonInvalid] Person
mkPerson''' (Right name) (Right age) = Right (Person name age)
mkPerson''' (Left err1) (Left err2) = Left (err1 ++ err2)
mkPerson''' (Left err1) _ = Left err1
mkPerson''' _ (Left err2) = Left err2




data Example a = Blah | Woot a deriving (Show)

-- Demonstrate that newtypes can still accept bottom values
newtype TestNewtype = TestNewtype Integer

foo :: TestNewtype -> String
foo x = "I returned"

data Identity a = Identity a

-- Doing `safeTail "julie"` -> Maybe [Char]

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs


data Trivial = Trivial deriving Show


data Unary a = Unary a deriving Show

-- The type of the variable below is: Num a => Unary a
-- i.e. a Unary holding a value of type Num a => a.
myUnary = Unary 10


-- Foo can store any type, but doesn't have a Show instance, so printing will
-- give an error.
data Foo a = Foo a

-- You can construct Unary (Foo 12) successfully.  But you can't print it,
-- because the derived Show instance will just try to call `show` on the contents.

-- This is acceptable type signature, but has no valid implementations
myFunction :: a -> f a
myFunction = undefined

-- UNFOLDS

limitedSeries = take 10 $ iterate (+1) 0

-- Apply with unfoldr.
unfoldFunction :: Integer -> Maybe (Integer, Integer)
unfoldFunction x = Just (x, x + 1)

-- Equivalent to the above but with a stop condition.
unfoldFunction' :: Integer -> Maybe (Integer, Integer)
unfoldFunction' x 
  | x > 10 = Nothing
  | otherwise = Just (x, x + 1)


-- Why bother with folds?  (Revision)

-- Tail recursive version of a sum function.
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go n [] = n
        go n (x:xs) = (go (n + x) xs)

-- But, using foldl we can do it more easily.  Look how simple this is and
-- it's still tail-recursive (well, it would be if we use foldl').
niceSum :: Num a => [a] -> a
niceSum xs = foldl (+) 0 xs


-- Tail recursive version of a product function.
mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go n [] = n
        go n (x:xs) = (go (n * x) xs)

niceProduct :: Num a => [a] -> a
niceProduct xs = foldl (*) 1 xs


-- A tail recursive concat pattern.
mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go acc [] = acc
        go acc (x:xs) = go (acc ++ x) xs

-- It's clear that this is much easier to write, once you've identified
-- the operator and the z-value.
niceConcat :: [[a]] -> [a]
niceConcat xs = foldl (++) [] xs






