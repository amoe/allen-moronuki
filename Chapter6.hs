module Chapter6 where

x = 42

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

instance Show a => Eq Date where
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



