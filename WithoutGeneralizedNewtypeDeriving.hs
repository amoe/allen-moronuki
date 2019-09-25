module WithoutGeneralizedNewtypeDeriving where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

-- Tail call to the instance of TooMany that is provided by Int.
instance TooMany Goats where
  tooMany (Goats n) = tooMany n
