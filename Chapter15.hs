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
