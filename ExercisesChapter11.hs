{-# LANGUAGE FlexibleInstances #-}
module ExercisesChapter11 where

import Data.Int
import Data.List (nub)

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
