{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

dtt1a = (* 9) 6

dtt1b = head [(0, "doge"), (1, "kitteh")]

dtt1c = head [(0 :: Integer, "doge"), (1, "kitteh")]

dtt1d = if False then True else False

dtt1e = length [1,2,3,4,5]


-- Here, w :: Num a => a
--x = 5
--y = x + 5
--w = y * 10

-- Here, z :: Num a => a -> a
--x = 5
--y = x + 5
--z y = y * 10

-- Here, f :: Fractional a => a
--x = 5
--y = x + 5
--f = 4 / y


-- Here, f :: [Char]
--x = "Julie"
--y = " <3 "
--z = "Haskell"
--f = x ++ y ++ z



