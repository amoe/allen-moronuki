module Chapter11 where

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
