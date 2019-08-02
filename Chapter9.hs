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

split :: Char -> String -> [String]
split _ "" = []
split x y = (:) element (split x rest)
  where isSep = (== x)
        isNonSep = (/= x)
        trimmed = dropWhile isSep y
        element = takeWhile isNonSep trimmed
        rest = dropWhile isNonSep trimmed


-- Single generator, single output function
lcVal1 = [ x^2 | x <- [1..10] ]

-- Single generator, single output function, single condition
lcVal2 = [ x^2 | x <- [1..10], rem x 2 == 0 ]

-- Multiple generators, they run in loops
lcVal3 = [ x^y | x <- [1..5], y <- [2, 3] ]

-- When does the condition run?  The condition seems to form a filter, as
-- expected.  It doesn't work in a stop-fast way, like takeWhile or similar
lcVal4 = [ x^y | x <- [1..10], y <- [2, 3], x^y < 200 ]

-- Getting tuple results.
lcVal5 = [ (x, y) | x <- [1, 2, 3], y <- [6, 7] ]

-- And the tuple doesn't have to have the same type.
lcVal6 = [ (x, y) | x <- [1, 2, 3], y <- ['a', 'b'] ]



mySqr = [ x^2 | x <- [1..10] ]

-- We can use the generated list as input to another list comprehension
lcVal7 = [ (x, y) | x <- mySqr, y <- [1..3], x < 4 ]
