module ExercisesChapter9 where

-- Ex: Comprehend thy lists

mySqr = [ x^2 | x <- [1..10] ]

lcVal1 = [ x | x <- mySqr, rem x 2 == 0 ]


wanted2 = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81), (4, 100), (9, 64), (9, 81), (9, 100), (16, 64), (16, 81), (16, 100), (25, 64), (25, 81), (25, 100), (36, 64), (36, 81), (36, 100), (49, 64), (49, 81), (49, 100)]

lcVal2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

wanted3 = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]

lcVal3 = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- Ex: Square cube

mySqr2 = [ x^2 | x <- [1..5] ]

myCube2 = [ y^3 | y <- [1..5] ]

someTuples1 = [ (x, y) | x <- mySqr2, y <- myCube2 ]

-- length someTuples2 = 15
someTuples2 = [ (x, y) | x <- mySqr2, y <- myCube2, x < 50, y < 50 ]


--- Ex: Bottom madness

expr1 = [ x^y | x <- [1..5], y <- [2, undefined] ]

expr2 = take 1 expr1

expr3 = sum [1, undefined, 3]

expr4 = length [1, 2, undefined]

expr5 = length $ [1, 2, 3] ++ undefined

expr6 = take 1 $ filter even [1, 2, 3, undefined]

expr7 = take 1 $ filter even [1, 3, undefined]

expr8 = take 1 $ filter odd [1, 3, undefined]

expr9 = take 2 $ filter odd [1, 3, undefined]

expr10 = take 3 $ filter odd [1, 3, undefined]


