module Chapter4 where

import GHC.Int

davesNot :: Bool -> Bool
davesNot True = False
davesNot False = True

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _ = Woot

someValue :: Integer
someValue = toInteger (minBound :: Int8)


davesTwo :: Integer
davesTwo = 2

davesFst :: (a, b) -> a
davesFst (a, b) = a

davesSnd :: (a, b) -> b
davesSnd (a, b) = b

