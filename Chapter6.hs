module Chapter6 where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

data Date = Date DayOfWeek Int deriving Show

instance Eq Date where
  (==) (Date weekDay1 dayOfMonth1) (Date weekDay2 dayOfMonth2) =
    weekDay1 == weekDay2 && dayOfMonth1 == dayOfMonth2

-- Pattern match(es) are non-exhaustive
-- In an equation for ‘f’:
--     Patterns not matched: p where p is not one of {2}
f :: Int -> Bool
f 2 = True

-- Pattern match(es) are non-exhaustive
-- In an equation for ‘g’:
--     Patterns not matched: p where p is not one of {3, 2}
g :: Int -> Bool
g 2 = True
g 3 = True

-- This compiles OK without warnings.
h :: Int -> Bool
h 2 = True
h _ = False

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = (x == x') && (y == y')

-- Union type
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False


foo :: String -> Float
foo _ = fromInteger 42

bar :: Ordering -> String
bar LT = "Less than"
bar GT = "Greater than"
bar EQ = "Equal"


-- This won't type check, because we don't have an instance of Eq so the
-- use of == is invalid.
check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

data MyType = THE_SINGLE_WAY_OF_WRITING_THIS_TYPE

instance Show MyType where
  show x = "Some representation"


--myVal :: String

-- Example of dispatching type class instances based on the type.

class Numberish a where 
  fromNumber :: Integer -> a
  toNumber :: a -> Integer


newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where summed = (toNumber a) + (toNumber a')

add :: Num a => a -> a -> a
add x y = x + y

-- Needs both Num AND Ord to type check, because we use > from Ord.
addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y = 
  if x > 1
  then x + y
  else x

-- We don't need to add any more types here because Int already implies its
-- various type class instances, in this case Eq.  But this is bad practice.
check'' :: Int -> Int -> Bool
check'' a a' = a == a'
