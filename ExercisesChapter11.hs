{-# LANGUAGE FlexibleInstances #-}
module ExercisesChapter11 where

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
