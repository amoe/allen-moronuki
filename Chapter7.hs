{-# LANGUAGE NoMonomorphismRestriction #-}
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

-- This is basically a reduce and is actually equivalent to sum.
f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

-- To write it in pointfree.
-- This won't compile with the monomorphism restriction active.
pointfreeF = foldr (+)


-- Stuff from the chapter definitions.

-- This is a binding.
blah :: Int
blah = 10

-- Example of an anonymous function being used.
bar = (*) ((\x -> x) 2) 2


-- Curry-prime because curry and uncurry are already in the prelude.

-- Curry goes from uncurried (tuple) style to curried style.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

-- An uncurried function.  Look that you can easily write in the uncurried
-- style, if you want, which is identical to the format used by other languags.
add :: (Int, Int) -> Int
add (a, b) = a + b


-- A curried function.
add' :: Int -> Int -> Int
add' a b = a + b

-- Pattern matching:

-- A 'nullary' data constructor that holds no values.
data Blah2 = Blah2

blahFunc :: Blah2 -> Bool
blahFunc Blah2 = True

-- A unary data constructor holding a single value.
data Identity a = Identity a deriving (Eq, Show)


-- We can unpack it with pattern matching.
unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x

-- We can also choose to ignore it
ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True

-- Or don't even bother actually pattern matching it.
ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True

-- An example of a product type.
data Product a b = Product a b deriving (Eq, Show)

productUnpackOnlyFirst :: Product a b -> a
productUnpackOnlyFirst (Product x _) = x

productUnpackOnlySecond :: Product a b -> b
productUnpackOnlySecond (Product _ x) = x

-- Or we can get both of them.
productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

data SumOfThree a b c = FirstPossible a | SecondPossible b | ThirdPossible c
  deriving (Eq, Show)

-- Discriminate based on members of a sum type.
sumToInt :: SumOfThree a b c -> Int
sumToInt (FirstPossible _) = 1
sumToInt (SecondPossible _) = 2
sumToInt (ThirdPossible _) = 3

-- Or ignore some members of a sum type if we feel like it.
sumToInt' :: SumOfThree a b c -> Int
sumToInt' (FirstPossible _) = 0
sumToInt' _ = 1

-- Bottom types

-- An infinite loop
argh x = argh x

-- Calling this with False notionally 'returns bottom'.
dontDoThis :: Bool -> Int
dontDoThis True = 1

-- The same with this, but this is NOT ADVISED to use error.
definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True = 1
definitelyDontDoThis False = error "oops"

-- The composition function... this is a valid implementation.
comp :: (b -> c) -> ((a -> b) -> (a -> c))
comp f g x = f (g x)

-- Pointfree examples.

-- Non-pointfree:

blah3 x  = x 
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)

-- Convert them to pointfree:
blah3' = id
addAndDrop' = const . (1+)
reverseMkTuple' = flip (,)
reverseTuple' = uncurry (flip (,))
