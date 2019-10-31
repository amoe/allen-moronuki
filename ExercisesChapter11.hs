{-# LANGUAGE FlexibleInstances #-}
module ExercisesChapter11 where

import Data.Int
import Data.List (nub)
import Data.Char (toUpper, isSpace, isUpper, toLower)
--import ModuleDemo (xyzzy, MyBool)

ch11 = 42

-- Type definitions for Vehicle exercise.

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size

-- Mini is a constant of type Manufacturer, so we apply the 2-arg data
-- constructor.
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

-- Use point free style.
areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
getManu _ = error "not a car"


-- Exercises: For Example
data Example = MakeExample deriving (Show);


data Example' = MakeExample' Int deriving (Show);


-- Exercises: Logic Goats

class TooMany a where
  tooMany :: a -> Bool

newtype TupleWrapper = TupleWrapper (Int, String) deriving (Show)

-- Pattern matching works fine inside the tuple.
instance TooMany TupleWrapper where
  tooMany (TupleWrapper (x, y)) = x > 42


newtype GoatsInTwoFields = GoatsInTwoFields (Int, Int) deriving (Show)

instance TooMany GoatsInTwoFields where
  tooMany (GoatsInTwoFields (x, y)) = (x + y) > 42

-- Difficult to test this.   Note that this part NEEDS FlexibleInstances.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y

-- a type with two data constructors.
-- It wraps a bool.
-- Think that the answer is (2 in first of sum type) + (2 in second)
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- Cardinality = 258
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- Exercises: How does your garden grow?

-- Non-normal form version:

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving (Show);

type Gardener = String
  
data Garden = Garden Gardener FlowerType deriving Show

-- Normal form version:
-- See Garden.hs, by definition, it has to live in a fresh module.

-- Operating system data.

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)


data ProgLang = 
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
  deriving (Eq, Show)

-- It's more easy to reorder fields when using this specific syntax.
nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

-- We can write the record fields in the constructor in a much different order
-- here.
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }







































allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [
  GnuPlusLinux,
  OpenBSDPlusNevermindJustBSDStill,
  Mac,
  Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [
  Haskell,
  Agda,
  Idris,
  PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]
  

-- Exercise: The Quad

data Quad = One | Two | Three | Four deriving (Eq, Show)


-- Has cardinality 8
eQuad :: Either Quad Quad
eQuad = undefined

-- Has cardinality 16
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- Has cardinality 4^4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined

-- Has cardinality 2*2*2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- Has cardinality (2^2)^2 = 16
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- Has cardinality (4^4)^2 = 65536
fTwo :: Bool -> Quad -> Quad
fTwo = undefined



























































data BinaryTree  a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
  


-- map but specifying a typeclass constraint on the function
ordmap :: (Ord a, Ord b) => (a -> b) -> [a] -> [b]
ordmap f xs = map f xs

addOne :: Integer -> Integer
addOne x = x + 10


-- Recurse down both sides and always replace the node contents
-- It's not clear that it stays balanced, though?
-- There must be an alternative way that uses insert' in the recursive call
mapTree :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)


testTree' :: BinaryTree Integer
testTree' = 
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = 
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- The test passes!
mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"

-- Write function to convert binary tree to a list.
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "Bad news bears."



-- So this works but looks insanely slow...
-- nm, according to larrybotha's repo this is at least the correct solution.
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = (++) ((++) [x] ll) rl
  where ll = preorder l
        rl = preorder r

-- This doesn't work because it recurses down the whole left side.
preorder' :: BinaryTree a -> [a]
preorder' Leaf = []
preorder' (Node Leaf x r) = (:) x (preorder' r)
preorder' (Node l x r) = (:) x (preorder' l)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r


postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l x r) = inorder l ++ inorder r ++ [x]


-- The regular type of foldr is
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
-- foldTree f z Leaf = z
-- foldTree f z (Node l x r) = f x (foldTree f z l) (foldTree f z r)

simplerTree :: BinaryTree Integer
simplerTree = Node (Node Leaf 18 Leaf) 2 (Node Leaf 7 Leaf)


-- In order to avoid having to combine the two 'b' values of the 2 branches, we
-- replace the z-value on the recursive call.  That will cause the bottomed-out
-- leaf value to end up yielding the replaced 'z' instead of 0.
-- This was fucking hard to figure out!
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node l x r) = f x (foldTree f (foldTree f z r) l)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

f Friday = "Miller Time"


g xs = xs !! (length xs - 1)












































-- As-patterns

-- An example of an as-pattern

-- t@X means that we capture the entire thing-that-matched as the variable 't'
-- inside the scope.  There's an equivalent thing in clojure

f' :: Show a => (a, b) -> IO (a, b)
f' t@(a, _) = do
  print a
  return t

-- No need to do it in weird actions. 
-- asPatternDemo [1,2,3] => [1,2,3,2,3]
asPatternDemo :: [a] -> [a]
asPatternDemo l@(x:xs) = l ++ xs

-- This will repeat the first item
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x:xs

-- The subsequence has to be in the original order
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False    -- Fail if we reached the end of ys
-- Test membership on the full list, but still recurse down
isSubseqOf xs'@(x:xs) ys'@(y:ys) = (elem x ys') && isSubseqOf xs ys

testInput = "hello world"


-- Use as-pattern to both take off the first character AND ALSO return the
-- unmodified version in the same pattern.
capitalizeHelper :: String -> (String, String)
capitalizeHelper xs'@(x:xs) = (xs', (toUpper x) : xs)

capitalizeWords :: String -> [(String, String)]
capitalizeWords x = map capitalizeHelper $ words x
  
capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs

sentenceInput = "blah. woot ha."


-- Not clear how to do this.  Something like takeWhile.  I don't think that
-- we know about any generic split function yet.  need to use isSpace

-- We need to know how to negate a function to do this properly
-- otherwise we'll have to repeat the separator stuff a lot.

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph x = capitalizeWord sentence ++ "." ++ capitalizeParagraph remaining
  where separatorChar = '.'
        isSeparator = (== separatorChar)
        isNotSeparator = (/= separatorChar)
        sentence = takeWhile isNotSeparator x
        remaining = dropWhile isSeparator $ dropWhile isNotSeparator x

-- Phone exercise

convo :: [String]
convo = 
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

type Presses = Int

-- In this case, 'a' should map to [('2', 1)] -- Press two once on the keypad.

-- DaPhone should be something that we can look up 'a' in.
-- Think about how we would support 'A'


-- It should be like:
-- if isupper:
--    Add 1 tap of '*'
--    Convert the char to lower  (not sure we can actually do this yet)
--    although there would be some hack involving ord/chr
--    Find the index -- not sure we've seen a function for this yet (recursive call?)
--Otherwise
--       Find the index

-- the fact that there is wrap around is interesting but doesn't apply to reverseTaps

type DaPhone = [(Char, String)]

thePhone = 
  [('1', "1"),
   ('2', "abc2"),
   ('3', "def3"),
   ('4', "ghi4"),
   ('5', "jkl5"),
   ('6', "mno6"),
   ('7', "pqrs7"),
   ('8', "tuv8"),
   ('9', "wxyz9"),
   ('0', " 0"),
   ('#', ".,")]


-- reversetaps goes from a char like 'c' to ('2', 3)
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c 
  | isUpper c = (:) ('*', 1) (reverseTaps p (toLower c))
  | otherwise = [reverseTapsForLowercase p c]

-- lowercase letter means always a single press, so no list needed in the type
reverseTapsForLowercase :: DaPhone -> Char -> (Digit, Presses)
reverseTapsForLowercase p c = (digit, (findIndex c spec) + 1)
  where (digit, spec) = lookupChar c p

-- This will be something like foldr with concat.
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p msg = foldr (++) [] $ map (reverseTaps p) msg

-- find index by predicate
findIndex :: Eq a => a -> [a] -> Int
findIndex y [] = -1
findIndex y (x:xs)
  | y == x = 0
  | otherwise = 1 + (findIndex y xs)
  

findByPredicate :: (a -> Bool) -> [a] -> a
findByPredicate _ [] = error "not found"
findByPredicate f (x:xs)
  | f x = x
  | otherwise = findByPredicate f xs


lookupChar c p = findByPredicate (\x -> elem c (snd x)) p


