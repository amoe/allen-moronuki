module MyReverse where

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (++) (myReverse xs) [x]

myReverse2 :: [a] -> [a] -> [a]
myReverse2 [] ys = ys
myReverse2 (x:xs) ys = myReverse2 xs ((:) x ys)

myReverse3 :: [a] -> [a]
myReverse3 xs = go xs []
  where go [] ys = ys
        go (x:xs) ys = go xs (x : ys)
