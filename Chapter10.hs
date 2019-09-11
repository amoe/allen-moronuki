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
foldrDemo = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" foldDemoXs

-- An example of how non-strict evaluation means that we don't have to evaluate
-- the entire spine of the list during traversal, depending on the folding
-- function.

-- The key here is the use of || which short-circuits, avoiding evaluating
-- the rest of the fold, which is passed as the second argument (shown here as
-- 'b'
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- Here, the fact that we don't reference y means that the rest of the fold
-- will never be evaluated at all.
-- However, the (x:xs) pattern match that is used is strict and forces the
-- evaluation of the first cons cell, meaning that (undefined ++ [1]) won't work.
-- y represents 'the result of having folded the rest of the list'.
weirdFn x y = 9001

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

foldlDemo = foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" foldDemoXs










































-- Demonstrate how foldl always evaluates the whole spine.


rcf = myFoldr (:) []
xs = [1,2,3] ++ undefined
evalDemo1 = take 3 $ rcf xs       -- Yep, works fine

lcf = myFoldl (flip (:)) [] 
evalDemo2 = take 3 $ lcf xs       -- Nope.  Definitely not fine


-- Works fine, we never evaluate const's second argument.
evalDemo3 = foldr const 0 ([1] ++ undefined)


-- Foldl will recurse to the final cons cell and evaluate the flipped const,
-- returning 5.
evalDemo4 = foldl (flip const) 0 [1..5]


xs0 = repeat 0 ++ [1,2,3]
xs1 = reverse xs0




































-- Demonstration of defining scanl.

-- infinite type attempt can happen when you have the wrong type signature.

-- The case expression here is only used to deduplicate the "q : <foo>" part.

myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : (case ls of
          [] -> []
          x:xs -> myScanl f (f q x) xs)

-- Rewritten without using case.
myScanl2 :: (a -> b -> a) -> a -> [b] -> [a]
myScanl2 f q [] = q : []
myScanl2 f q (x:xs) = q : (myScanl2 f (f q x) xs)


-- Produce the fibs by a scanl.  (!)
fibs = 1 : scanl (+) 1 fibs

nthFib :: Int -> Integer
nthFib n = fibs !! n
