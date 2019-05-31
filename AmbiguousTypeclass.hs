module AmbiguousTypeclass where

newtype Age = Age Integer deriving (Eq, Show)
newtype Year = Year Integer deriving (Eq, Show)

class Numberish2 a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

instance Numberish2 Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

instance Numberish2 Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

-- The above has an ambiguous value in the typeclass.
-- You can't write `defaultNumber` -- this will fail to typecheck.
-- You have to hint which value you want.

-- eg, defaultValue :: Year
-- This gives you the value defined within the 'Year' type's instance of 
-- Numberish2.
