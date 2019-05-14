module Chapter6 where

x = 42

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
