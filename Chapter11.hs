module Chapter11 where

import Data.Int (Int8)

ch11 = 42

data MyBool = MyFalse | MyTrue

data Trivial = Trivial'

data UnaryTypeCon a = UnaryValueCon a


-- Data constructors and values

-- PugType is a type constant, PugData is a constant value.
data PugType = PugData

-- Phantom type variable.  It does not appear in any data constructor.
data HuskyType a = HuskyData

-- Type variables do not need to have single character names.
data DogueDeBordeaux doge = DogueDeBordeaux doge

-- The type hint is not technically needed here.
myPug = PugData :: PugType

-- Or here tbh
myHusky :: HuskyType a
myHusky = HuskyData

-- Put a type class constraint on this husky.  It holds a Num.
-- Despite not having a 'witness' for its type variable a.
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)


-- What's a type and what's data?

-- The type 'Integer' is a "type argument" to the DATA constructor.
-- Because the type Integer is fixed at compile time, there are no type variables.
-- Therefore the type constructor `Price` can be called a "type constant".
data Price = Price Integer deriving (Eq, Show)

-- A sum type with 3 data constructors.  No arguments.
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)


-- Same
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

-- Here, Car takes 2 type arguments (Manufacturer and Price).
-- Plane takes one type argument (Airline).
data Vehicle = Car Manufacturer Price | Plane Airline


-- It's important to realize that some stuff is being GENERATED: Car and Plane
-- are generated; in contrast, the type arguments to the data constructors
-- (Manufacturer, Price, Airline) must ALREADY EXIST in the compilation context.

-- In a way, it's too late to get this lesson so sharply, because I already
-- realized it.


-- Arities of data constructors

-- Nullary
data Example0 = Example0 deriving (Eq, Show)

-- Unary data constructor
data Example1 = Example1 Int deriving (Eq, Show)

-- Binary data constructor and therefore also a product type.
data Example2 = Example2 Int String deriving (Eq, Show)

-- A unary data constructor (MyVal)
data MyType = MyVal Int deriving (Eq, Show)

-- The type of MyVal is `Int -> MyType`.  There is 1 arrow, therefore there is
-- 1 argument.

-- Calculate the cardinality, but because we overflow the Int8 type we must use
-- toInteger to up-cast at runtime.
cardinalityOfInt8 :: Integer
cardinalityOfInt8 = (negate arche) + 1 + telos
  where arche = toInteger $ (minBound :: Int8)
        telos = toInteger $ (maxBound :: Int8)

cardinalityOfInt :: Integer
cardinalityOfInt = (negate arche) + 1 + telos
  where arche = toInteger $ (minBound :: Int)
        telos = toInteger $ (maxBound :: Int)


-- The value produced by the data constructor `MakeExample` inhabits the type
-- `Example`.  The cardinality of the type `Example` is 1.
data Example = MakeExample deriving (Show);

-- data Goats = Goats Int deriving (Eq, Show);


-- 43 goats is too many.
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

-- Declare a wrapper type for Goats.  It might be better called GoatLimitation?
tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42  

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Despite really being an Int, we can still define different type class instances
-- for goats.
instance TooMany Goats where
  tooMany (Goats n) = n > 64
  

type Goats' = Int

-- Won't work
-- instance TooMany Goats' where
--   tooMany n = n > 64
