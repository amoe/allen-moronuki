module Chapter15 where

import Data.Monoid
import Test.QuickCheck

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

  
evilPlus = flip (+)

evilPlusPlus = flip (++)


type Exclamation = String
type Adverb = String
type Noun = String  
type Adjective = String
  

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin excl adv noun adj = excl <> "! he said " <> adv
                               <> " as he jumped into his car " <> noun
                               <> " and drove off with his " <> adj <> " wife"

  
-- madlibbinBetter "hey ho" "loudly" "door" "fat"
madlibbinBetter :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter excl adv noun adj = mconcat
                                    [excl, "! he said ", adv,
                                     " as he jumped into his car ", noun,
                                     " and drove off with his ", adj, " wife"]
  


-- Check associativity of simple arithmetic expresssions  
isAddAssociative a b c = a + (b + c) == (a + b) + c
isMulAssociative a b c = a * (b * c) == (a * b) * c


isOperationAssociative f x y z = result1 == result2
  where result1 = f x (f y z)
        result2 = f (f x y) z
  


-- Binds infix <> *inside the definition* to whatever was passed in, shadowing
-- the definition from Semigroup.  Impressive but confusing.
isOperationAssociative' :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
isOperationAssociative' (<>) a b c = a <> (b <> c) == (a <> b) <> c


-- Encapsulate the monoid associativity law.  
isMonoidAssociative :: (Eq m, Monoid m) => m -> m -> m -> Bool
isMonoidAssociative a b c = a <> (b <> c) == (a <> b) <> c


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

quickcheckMain :: IO ()
quickcheckMain = do
  -- Test that the law holds for integers
  verboseCheck (isMonoidAssociative :: Sum Integer -> Sum Integer -> Sum Integer -> Bool)
  verboseCheck (monoidRightIdentity :: Sum Integer -> Bool)
  verboseCheck (monoidLeftIdentity :: Sum Integer -> Bool)
--  verboseCheck isMonoidAssociative

  

data Bull = Fools | Twoo  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (2, return Twoo)]

-- Under our fictional invalid monoid, we always return Fools in any monoidal
-- combination.  This wouldn't be correct because in that case the identity
-- property would be violated.  We do this to show that QuickCheck can enforce
-- properties that the type system itself cannot.
instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

qcBull = do
  -- It's associative, because it will always return `Fools` so the ordering
  -- does not matter.
  quickCheck (isMonoidAssociative :: Bull -> Bull -> Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
  
