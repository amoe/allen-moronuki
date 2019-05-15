module Chapter6 where

x = 42

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

data Date = Date DayOfWeek Int

instance Eq Date where
  (==) (Date weekDay1 dayOfMonth1) (Date weekDay2 dayOfMonth2) =
    weekDay1 == weekDay2 && dayOfMonth1 == dayOfMonth2
