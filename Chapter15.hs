module Chapter15 where

ch15 = "foo"

-- mappend implements ++ when applied to strings, which are just [Char].  
fry = " goes well with garlic"
fry' = mappend "Trout" fry

-- Use monoid operations to implement 'concat'.  
bender = [[1,2,3], [4,5,6]]
bender' = foldr mappend mempty bender
