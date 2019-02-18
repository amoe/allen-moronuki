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

-- It's a two argument function that is going to combine its tuple arguments
-- in a certain way.
tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a, b) (c, d) = ((a + c), (b ++ d))
