module Chapter10 where

ch10 = 42

dave1 = foldr (+) 0 [1..10]

myFoo :: [a] -> [] a
myFoo xs = map id xs


-- We can rebind the official foldr to this signature, if we don't need the
-- abstractness provided by the Foldable type class.
listOnlyFoldr :: (a -> b -> b) -> b -> [] a -> b
listOnlyFoldr = foldr
