module ExercisesChapter14 where

import Test.Hspec
import WordNumber (digitToWord, getIndividualDigits, wordNumber)
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)
  
hspecMain :: IO ()
hspecMain = hspec $ do
  describe "digitToWord" $ do
    it "return zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "return one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "getIndividualDigits" $ do
    it "returns [1] for 1" $ do
      getIndividualDigits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      getIndividualDigits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  

--   describe "QuickCheck usage" $ do
--     it "x + 1 is always greater than x" $ do
-- --      property $ \x -> (x + 1) > (x :: Int)
--       property $ f


half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = (half x) * 2 == x

-- How does this work?  It's a right-fold which keeps some status in a tuple
-- over xs.  The tuple is (LAST-CHAR, CURRENT-STATUS).
-- The input value 'comes in' from the first argument.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status    -- If we ever saw False, it means one pairing was wrongturn false forever.
        go y (Nothing, t) = (Just y, t)    -- If the first turn through the loop, store our last-seen-value and stay True.  [_] -> True.
        go y (Just x, t) = (Just y, x >= y) -- Compare to last-seen-value and turn False if wrongly ordered.


prop_listOrdered :: [Integer] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

-- If we just type hint this properly, it automatically becomes a property.
prop_plusAssociative :: Integer -> Integer -> Integer  -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multiplyAssociative :: Integer -> Integer -> Integer  -> Bool
prop_multiplyAssociative x y z = x * (y * z) == (x * y) * z

prop_multiplyCommutative :: Integer -> Integer  -> Bool
prop_multiplyCommutative x y = x * y == y * x

-- Testing this property will fail!  Because subtraction is not associative.
prop_subtractAssociative :: Integer -> Integer -> Integer  -> Bool
prop_subtractAssociative x y z = x - (y - z) == (x - y) - z

prop_subtractCommutative :: Integer -> Integer -> Bool
prop_subtractCommutative x y = x + y == y + x

-- This will cause a divide by zero if attempted to be tested using simply the
-- regular generator for Integers.  
quotLaw :: Integer -> Integer -> Bool
quotLaw x y = (quot x y) * y + (rem x y) == x

divLaw :: Integer -> Integer -> Bool
divLaw x y = (div x y) * y + (mod x y) == x

  
-- Kind of nasty, but we don't know any other syntax to produce nonzero
-- integers.
genNonzero :: Gen Integer
genNonzero = elements nonzeroIntegers
  where nonzeroIntegers = [1..100] ++ [(-100)..(-1)]

-- Using the new Gen that we just defined in place of the standard 'arbitrary'.
genNonzeroTuple :: Gen (Integer, Integer)
genNonzeroTuple = do
  a <- genNonzero
  b <- genNonzero
  return (a, b)

-- Since we've only been shown how to get single values from generators, but we
-- need two values for one application of this function, we uncurry the quotLaw
-- so that it accepts a single 2-tuple of the form that our Gen produces.
prop_quotLaw :: Property
prop_quotLaw = forAll genNonzeroTuple $ uncurry quotLaw


prop_divLaw :: Property
prop_divLaw = forAll genNonzeroTuple $ uncurry divLaw

-- Falsifiable for 1^0 != 0^1
prop_exponentiationCommutative :: Integer -> Integer -> Bool
prop_exponentiationCommutative x y = x ^ y == y ^ x

-- Falsifiable:
-- 0 ^ (0 ^ 0) == 0  because 0 x 1 = 0
-- (0 ^ 0) ^ 0 == 1  because 1^0 = 1
prop_exponentiationAssociative :: Integer -> Integer -> Integer -> Bool
prop_exponentiationAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

  
-- Fun fact -- this is called an 'involution' mathematically.
prop_twiceReverseIdentity :: [Integer] -> Bool
prop_twiceReverseIdentity xs = (reverse . reverse) xs == xs

-- The dollar sign
prop_infixApplication :: (Eq b) => (a -> b) -> a -> Bool
prop_infixApplication f x = (f $ x) == (f x)

-- Quickcheck can't infer the first argument, so we need to specialize the
-- property, which still remains a property after partial application.
prop_infixApplyAddOne :: Integer -> Bool
prop_infixApplyAddOne = prop_infixApplication (+1)

prop_composeRelation :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_composeRelation f1 f2 x = ((f1 . f2) x) == (f1 (f2 x))

prop_composeRelationAddOne :: Integer -> Bool
prop_composeRelationAddOne = prop_composeRelation (+1) (+1)  

-- Falsifiable with [0] and [1]; f = [1,0], (++) = [0,1]
prop_consFoldEqualsConcatenationOperator :: (Eq a) => [a] -> [a] -> Bool
prop_consFoldEqualsConcatenationOperator xs ys = (f xs ys) == ((++) xs ys)
  where f = foldr (:)


-- I believe that these should be equal.

-- g needs to have its type fixed thus, or it won't compile; yet it does
-- compile OK at the ghci repl for some reason.
prop_foldConcatOperatorEqualsConcat :: (Eq a) => [[a]] -> Bool
prop_foldConcatOperatorEqualsConcat xs = (f xs) == (concat xs)
  where
    f :: [[a]] -> [a]
    f = foldr (++) []

-- Clearly does not hold because the number of the list may be shorter than the
-- number requested.  Falsifiable for 1  and [].
prop_takeYieldsRequestedNumber :: Int -> [a] -> Bool
prop_takeYieldsRequestedNumber n xs = (f n xs) == n
  where f n xs = length (take n xs)


prop_readShowRoundTrip :: (Show a, Read a, Eq a) => a -> Bool
prop_readShowRoundTrip x = (f x) == x
  where f x = read $ show x
  

square x = x * x
  
squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

-- Falsifiable, due to inaccuracy of floating point arithmetic.
prop_squareIdentity :: (Eq a, Floating a) => a -> Bool
prop_squareIdentity x = (squareIdentity x) == x


-- Idempotence

twice f = f . f
fourTimes = twice . twice

-- This test actually exposed a bug in this function, which didn't originally
-- support the empty string.
capitalizeWord :: String -> String
capitalizeWord [] = ""  
capitalizeWord (x:xs) = (toUpper x) : xs

prop_capitalizeWordIdempotent :: String -> Bool  
prop_capitalizeWordIdempotent x =
  (capitalizeWord x == twice capitalizeWord x)
  && (capitalizeWord x == fourTimes capitalizeWord x)


prop_sortIdempotent :: Ord a => [a] -> Bool  
prop_sortIdempotent xs =
  (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)
  
-- f x = (capitalize-dwim
  
quickcheckMain :: IO ()
quickcheckMain = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplyAssociative
--  quickCheck prop_subtractAssociative
  quickCheck prop_subtractCommutative
  quickCheck prop_quotLaw
  quickCheck prop_divLaw
--  quickCheck prop_exponentiationCommutative
--  quickCheck prop_exponentiationAssociative
  quickCheck prop_twiceReverseIdentity
  quickCheck prop_infixApplyAddOne
  quickCheck prop_composeRelationAddOne
--  quickCheck (prop_consFoldEqualsConcatenationOperator :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_foldConcatOperatorEqualsConcat :: [[Integer]] -> Bool)
  -- quickCheck (prop_takeYieldsRequestedNumber :: Int -> [Integer] -> Bool)
  quickCheck (prop_readShowRoundTrip :: Integer -> Bool)
  quickCheck (prop_readShowRoundTrip :: String -> Bool)
  quickCheck (prop_readShowRoundTrip :: Float -> Bool)
--  quickCheck (prop_squareIdentity :: Double -> Bool)
  quickCheck prop_capitalizeWordIdempotent
  quickCheck (prop_sortIdempotent :: [Integer] -> Bool)

-- "Make a Gen random generator for the datatype".

-- Equal probabilities for each.  
data Fool = Fulse | Frue deriving (Eq, Show)

-- Use oneof to create generators for sum types.
genFoolEqual :: Gen Fool
genFoolEqual = do
  oneof [return $ Fulse, return $ Frue]


-- We want: 2/3 Fulse, 1/3 Frue.
-- 1 and 1 will give both equal weight.  Chance is 1/2.
-- 2 and 1 therefore gives Frue 1/3 and Fulse 2/3.
-- But does it make sense to say that a 2/3 chance is "twice as likely" as a 1/3
-- chance?  Yes; you multiply the numerators.
genFoolWeighted :: Gen Fool
genFoolWeighted = do
  frequency [(2, return $ Fulse), (1, return $ Frue)]
  
-- Testing Hangman functions.  Using my implementations rather than A&M's
-- implementations.

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Show, Eq)

flipIfCorrect :: Char -> (Char, Maybe Char) -> Maybe Char
flipIfCorrect guessed (real, currentStatus) = if guessed == real
                                              then (Just real)
                                              else currentStatus

-- What is this function supposed to do?
-- It takes a Puzzle, and a candidate Char.
-- The Puzzle holds a [Maybe Char] representing the stuff that has been guessed.
-- It also holds the actual answer to the puzzle as its first data member.
-- It also holds a list of stuff-already-guessed, which we need to make sure gets
-- our guess appended to.  
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle x y z) l = Puzzle x y' z'
  where y' = map (flipIfCorrect l) $ zip x y
        z' = (l : z)


charInWord :: Char -> Puzzle -> Bool
charInWord x (Puzzle x' _ _) = elem x x'

alreadyGuessed :: Char -> Puzzle -> Bool
alreadyGuessed x (Puzzle _ _ x') = elem x x'

-- What does this function do?
-- It takes a guess and a puzzle, does some IO, and yields a Puzzle inside the
-- IO monad.    The test will be the same, except with one more case.  Will it be
-- complicated by the fact that the result is an IO Puzzle?
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord guess puzzle, alreadyGuessed guess puzzle) of
    (_, True) -> do
      putStrLn "Duplicate guess, bozo."
      return puzzle
    (True, _) -> do
      putStrLn "You Guessed Correctly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "The guess was wrong."
      return (fillInCharacter puzzle guess)
  
  
hangmanSpecMain :: IO ()
hangmanSpecMain = hspec $ do
  describe "fillInCharacter" $ do
    it "flips the value for a correct guess" $ do
      let x = Puzzle "foo" [Nothing, Nothing, Nothing] []
          y = 'f'
          expected = Puzzle "foo" [Just 'f', Nothing, Nothing] ['f'] in
        fillInCharacter x y `shouldBe` expected
    it "does not flip the value for an incorrect guess" $ do
      let x = Puzzle "foo" [Nothing, Nothing, Nothing] []
          y = 'z'
          expected = Puzzle "foo" [Nothing, Nothing, Nothing] ['z'] in
        fillInCharacter x y `shouldBe` expected
  describe "handleGuess" $ do
    it "fills in the value for a correct guess" $ do
      let x = Puzzle "foo" [Nothing, Nothing, Nothing] []
          y = 'f'
          expected = Puzzle "foo" [Just 'f', Nothing, Nothing] ['f'] in
        do
          -- Bind the IO value so that we can compare it to our non-IO Puzzle.
          actual <- handleGuess x y
          actual `shouldBe` expected
