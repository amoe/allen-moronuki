module Fib where

xFib = 42

-- Dave's attempt at fib.
nthFib :: Integer -> Integer
nthFib 0 = 0
nthFib 1 = 1
nthFib n = f1 + f2
  where f1 = nthFib (n - 1)
        f2 = nthFib (n - 2)
