module Chapter15 where

import Data.Monoid

ch15 = "foo"

-- mappend implements ++ when applied to strings, which are just [Char].  
fry = " goes well with garlic"
fry' = mappend "Trout" fry

-- Use monoid operations to implement 'concat'.  
bender = [[1,2,3], [4,5,6]]
bender' = foldr mappend mempty bender

-- Use monoidal operations to sum a list of integers
theIntegers = [3, 5, 7]

-- How do we get the value 'out' of the newtype?  We can use pattern matching.
-- or we can also use getSum.  
theSum = getSum $ mconcat $ map Sum theIntegers

xs = [2, 4, 6] :: [Product Int]

-- These values are identical for any arguments, demonstrating the associativity
-- law of monoids.  
rightAssociativeSum = (<>) (Sum 1) ((<>) (Sum 2) (Sum 3))
leftAssociativeSum = (<>) ((<>) (Sum 1) (Sum 2)) (Sum 3)


rightAssociativeSum' = Sum 1 <> (Sum 2 <> Sum 3)
leftAssociativeSum' = (Sum 1 <> Sum 2) <> Sum 3


-- This works to test if all values are true
boolValues = map All [True, False, True]
allWereTrue = mconcat boolValues


firstMaybe = mconcat $ map First $ [Nothing, Just 42, Nothing, Just 43, Nothing]
lastMaybe = mconcat $ map Last $ [Nothing, Just 42, Nothing, Just 43, Nothing]
  

-- This is called a phantom type; the type argument does not appear anywhere
-- in the data constructors.  
data Booly a = False' | True' deriving (Eq, Show)

instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'

instance Monoid (Booly a) where
  mempty = True'
