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



-- We can specify type class constraints on multiple type variables.
-- However this severely impedes our ability to actually do anything with
-- the argument.  For instance, we can't add x and y.  Why?  Because + can only
-- add values of the SAME type.  (You can confirm this because its type signature
-- only contains one type variable.)

-- However, note that in this example, the result type doesn't get fixed to a
-- concrete type.
--
-- > :t davesFunction 1 2
-- davesFunction 1 2 :: Num b => b
--
-- It stays with the type-class-constraint associated to its type variable.

davesFunction :: (Num a, Num b) => a -> b -> b
davesFunction _ y = y


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


-- Demos of parametric polymorphism

davesId :: a -> a
davesId x = do
  x
  where
    greeting = "Hello"

davesMysteryFunction1 :: a -> a -> a
davesMysteryFunction1 x y = x


davesMysteryFunction2 :: a -> a -> a
davesMysteryFunction2 x y = x

davesPuzzle :: a -> b -> b
davesPuzzle x y = y







































-- ourId gets inferred; a -> a
ourId x = x

-- myGreet gets inferred; [Char] -> [Char]
myGreet x = x ++ "Julie"

-- myGreet2 gets inferred; [a] -> [a] -> [a]
myGreet2 x y = x ++ y
