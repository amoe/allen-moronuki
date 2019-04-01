module TypeInference1 where

-- Here, the presence of the numeric literal 3, which has a type of Num a => a,
-- forces the other arguments to have this type.
f :: (Num a) => a -> a -> a
f x y = x + y + 3
