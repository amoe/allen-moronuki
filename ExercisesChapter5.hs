{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercisesChapter5 where

-- Does it compile?  1

-- 1, fixed version:
bigNum = (^) 5
wahoo = bigNum $ 10


-- Does it compile?  2

x = print
y = print "woohoo!"
z = x "hello world"

-- Does it compile?  3

-- Fixed version:
a0 = (+)
b0 = 5
c0 = a0 b0 10
d0 = a0 c0 200

-- Does it compile?  4

-- a1 = 12 + b1
-- b1 = 10000 * c1

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r x = x ++ x

co :: (b -> c) -> (a -> b) -> a -> c
co f1 f2 x = f1 (f2 x)


a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f1 x = f1 x  
