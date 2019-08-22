module Chapter10 where

ch10 = 42

dave1 = foldr (+) 0 [1..10]

myFoo :: [a] -> [] a
myFoo xs = map id xs


-- We can rebind the official foldr to this signature, if we don't need the
-- abstractness provided by the Foldable type class.
listOnlyFoldr :: (a -> b -> b) -> b -> [] a -> b
listOnlyFoldr = foldr


myFoldr :: (a -> b -> b) -> b -> [] a -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (foldr f z xs)


-- A more schemey definition.
myFoldr2 :: (a -> b -> b) -> b -> [] a -> b
myFoldr2 f z xs = 
  case xs of
    [] -> z
    (x:xs) -> f x (foldr f z xs)

-- Demonstration of how folds evaluate by showing how the rewriting actually
-- happens.
foldDemoXs = map show [1..5]
foldDemoY = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" foldDemoXs

-- An example of how non-strict evaluation means that we don't have to evaluate
-- the entire spine of the list during traversal, depending on the folding
-- function.

-- The key here is the use of || which short-circuits, avoiding evaluating
-- the rest of the fold, which is passed as the second argument (shown here as
-- 'b'
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs
