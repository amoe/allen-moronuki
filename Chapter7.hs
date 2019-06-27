module Chapter7 where

myNum :: Integer
myNum = 1

myVal f = myNum

myVal2 f = f + myNum

-- The type that ghc infers for this is:
-- t -> t1 -> Integer
-- Note that the two types can potentially differ.
myVal3 f g = myNum

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was " ++ show x ++ " and y was " ++ show y

bindExp2 :: Integer -> String
bindExp2 x = 
  let x = 10
      y = 5 in
    "the integer was " ++ show x ++ " and y was " ++ show y

triple = \x -> x * 3


foo = (\x -> x * 3) 3

-- Now a bunch of lambda exercises, then back to pattern matching.

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

data Mood = Blah | Woot

isItBlah :: Mood -> Bool
isItBlah Blah = True
isItBlah _ = False


-- User printing example
newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "<Unregistered>"
prwintUser (RegisteredUser (Username u) (AccountNumber a)) = putStrLn (u ++ ": " ++ show a)



-- Penguin example

data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica
  deriving (Eq, Show)

-- A 'product type', like it's just a wrapper for another data constructor.
data Penguin = Peng WherePenguinsLive

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng x) = x

-- Test data

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

isGalapagosPenguin :: Penguin -> Bool
isGalapagosPenguin (Peng Galapagos) = True
isGalapagosPenguin (Peng _) = False

isAntarcticPenguin :: Penguin -> Bool
isAntarcticPenguin (Peng Antarctica) = True
isAntarcticPenguin (Peng _) = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (isGalapagosPenguin p) || (isAntarcticPenguin p)

funcZ :: (Eq a, Num a) => a -> String
funcZ x = 
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal :: (Eq a) => [a] -> String
pal xs = 
  case xs == reverse xs of
    True -> "yes"
    False -> "no"


-- Demo that we can use some expression to swap the vars, and then later pattern
-- match with a case expression.
swapAndCase tpl = 
  case (snd tpl, fst tpl) of
    (x, y) -> x


myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ x = x

-- This is identical to the previous one, but the type signature denotes the
-- currying explicitly.
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ x = x

-- This will NOT work, as this would denote a single-parameter function.
-- • The equation(s) for ‘returnLast''’ have four arguments,
--   but its type ‘(((a -> b) -> c) -> d) -> d’ has only one
--returnLast'' :: (((a -> b) -> c) -> d) -> d
--returnLast'' _ _ _ x = x

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f x _ = f x































myAbs :: Integer -> Integer
myAbs x 
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"


-- Will not work, because arbitrary exprs are not supported by case.
-- bloodNaCase :: Integer -> String
-- bloodNaCase x = case x of
--   x < 135 -> "too low"
--   _ -> "fallback"

-- Bit of pythag
isRightIf :: (Num a, Eq a) => a -> a -> a -> String
isRightIf x y z = if (x * x) == ((y * y) + (z * z)) then "right" else "not right"

-- Note that we can use the caret to do exponentiation.
isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c 
  | (a^2 + b^2) == c^2 = "right"
  | otherwise = "not right"

dogYears :: Integer -> Integer
dogYears x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100



























