module Chapter9 where


data MyList a = EmptyList | MyList a (MyList a)
  deriving (Show)

ch9foo = 42

-- Pattern matching on the infix data constructor of lists.
-- The infix data constructor is called (:), aka 'cons'.
-- These functions are partial though, and they will create errors if they
-- are passed empty list.

myHead (x : _) = x
myTail (_ : xs) = xs

-- myTail can be fixed by adding a base case.  However, that wouldn't work
-- for myHead, because it would no longer have the type [a] -> a.
myTail2 [] = []
myTail2 (_ : xs) = xs


-- Tail function can also be fixed by wrapping the type with Maybe.
myTail3 :: [a] -> Maybe [a]
myTail3 [] = Nothing
myTail3 (_ : xs) = Just xs

-- Apply the same fix to the head.
myHead3 :: [a] -> Maybe a
myHead3 [] = Nothing
myHead3 (x : _) = Just x



-- eft is an abbreviation of enumFromTo


-- For Bool we can just handle all possible cases by pattern matching
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True False = []
eftBool True True = [True]

-- Create the ordering by consing the items one by one.
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x > y = []     -- Degenerate error case
  | x == y = [y]
  | otherwise = (:) x (eftOrd (succ x) y)

-- Exactly the same
eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [y]
  | otherwise = (:) x (eftInt (succ x) y)

-- And also exactly the same
eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x == y = [y]
  | otherwise = (:) x (eftChar (succ x) y)

l = ["foo", "bar", "baz"]

isSpace = (== ' ')
isNonSpace = (/= ' ')

myWords :: String -> [String]
myWords "" = []
myWords s = (:) word (myWords rest)
  where trimmed = dropWhile isSpace s
        word = takeWhile isNonSpace trimmed
        rest = dropWhile isNonSpace trimmed

