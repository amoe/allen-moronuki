{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WithGeneralizedNewtypeDeriving where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Now we can derive TooMany off the wrapped Int without boilerplate.
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

