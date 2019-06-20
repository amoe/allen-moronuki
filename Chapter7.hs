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

