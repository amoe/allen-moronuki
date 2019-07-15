module Chapter8 where

x = 42

-- The 'silly way':
fourFactorial :: Integer
fourFactorial  = 4 * 3 * 2 * 1

fac :: Integer -> Integer
fac 1 = 1
fac x = x * fac (x - 1)

brokenFac :: Integer -> Integer
brokenFac x = x * brokenFac (x - 1)

-- This signature is ok because of currying, although this LOOKS like a 3 arg
-- function, it's actually equivalent to a two-arg function.
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g = \x -> f (g x)


-- defining inc in a pointfree style
inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
three' = (inc . inc . inc) 0

-- Eq has to be in the type signature because the 0 in the pattern must be
-- compared.  Note that this is not wrong but this is the tail-recursive
-- version.
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes seed 0 = seed
incTimes seed n = incTimes (inc seed) (n - 1)

-- A more general way of applying any unary function n times.  (head-recursive)
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f seed = seed
applyTimes n f seed = f (applyTimes (n - 1) f seed)

-- The tail version (my addition)
applyTimesTail :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimesTail 0 f seed = seed
applyTimesTail n f seed = applyTimesTail (n - 1) f (f seed)

-- With explicit composition (semantically the same)
applyTimesComp :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimesComp 0 f seed = seed
applyTimesComp n f seed = (f . (applyTimesComp (n - 1) f)) seed
