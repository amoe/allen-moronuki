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


allLowercase = ['a'..'z']


-- Blind attempt at removeLowercase
removeLowercase :: String -> String
removeLowercase xs = [ x' | x' <- xs, not (elem x' allLowercase) ]


-- Or the inverted logic version, but note that this removes whitespace.
keepUppercase :: String -> String
keepUppercase xs = [ x' | x' <- xs, elem x' ['A'..'Z'] ]

-- A&M ask us what this expression will do, the answer is that it will keep the
-- vowels from the string.
keepVowels :: String -> String
keepVowels xs = [ x' | x' <- xs, elem x' "aeiou" ]

-- Normal form

-- This expression is in NF (and WHNF).
expr1 = (1, 2)

-- This expression is merely in WHNF.  The outer expression is a data constructor.
expr2 = (1, 1 + 1)

-- This expression is in NF.  It can't be reduced because of the free variable x.
expr3 = \x -> x * 10

-- This expression is in none of them!  The outer expression is a function
-- application.  It's in a non-reduced state.
expr4 = "Papu" ++ "chon"

-- This expression is in WHNF
expr5 = (1, "Papu" ++ "chon")

-- This list is in NF.
expr6 = [1, 2, 3]

weirdList1 = [1] ++ undefined ++ [3]
weirdList2 = [1] ++ [undefined] ++ [3]

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs



mapped1 = map (+1) [1, 2, 3, 4]
mapped2 = map (1-) [1, 2, 3, 4]
mapped3 = fmap (+1) [1, 2, 3, 4]
mapped4 = fmap (2*) [1, 2, 3, 4]
mapped5 = map id [1,2,3,4]
mapped6 = fmap id [1,2,3,4]

-- Map works with any function.
mapped7 = map fst [(2,3), (4,5), (6,7)]

-- Including partially applied functions.
mapped8 = map (take 3) [[1..5], [1..5], [1..5]]

-- And including anonymous functions.
mapped9 = map (\x -> x + 1)  [1..5]

-- And you can do complex expressions.  note that the type of (a -> b) can still
-- be inferred here.
mapped10 = map (\x -> if x == 3 then negate x else x)  [1..5]


filtered1 = filter even [1..10]
filtered2 = filter (== 'a') "abracadabra"
filtered3 = filter (\x -> (rem x 2) == 0) [1..10]
filtered4 = [x | x <- "abracadabra", elem x "aeiou"]

zipped1 = zip [1,2,3] [4,5,6]

-- Does not fail, just produces a list of length 2
-- (the length of the shortest list)
zipped2 = zip [1,2,3] [4,5]

-- Completes quickly
zipped3 = zip [] [1..100000]

-- We can also unzip.
unzipped1 = unzip [(1, 4), (2, 5), (3, 6)]

-- Note here again the use of the argument/tuple symmetry, as unzip yields a
-- 2-tuple.

zw1 = zipWith (+) [1, 2, 3] [10, 11, 12]

zw2 = zipWith (*) [1, 2, 3] [10, 11, 12]

-- Should just be true shurely -- and indeed it is.
zw3 = zipWith (==) ['a'..'f'] ['a'..'m']
