module Chapter9 where


data MyList a = EmptyList | MyList a (MyList a)
  deriving (Show)

ch9foo = 42
