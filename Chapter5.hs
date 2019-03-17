module Chapter5 where

x = 42

--addStuff :: Integer -> Integer -> Integer
-- More explicit type signature.
addStuff :: Integer -> (Integer -> Integer)
addStuff a b = a + b + 5

-- Not quite sure what this is intended to prove.

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne = subtractStuff 1


davesAddition :: Num a => a -> a -> a
davesAddition x y = x + y

-- This type signature
-- davesAddition2 :: (Num a, Num b) => a -> b -> b
-- davesAddition2 x y = x + y


nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b  = (i + nonsense b)

-- This works the same.  We destructure the tuple in the term.  It needs the
-- slightly uglier call syntax.
uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = (i + nonsense b)

-- This shows that the lambda syntax can handle implicit currying, ie we create
-- a two-arg lambda here.
anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

-- This form is showing the "real" expansion of the implicit currying.
anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)
