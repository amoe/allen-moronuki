sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")


triple x = x * 3

half x = x / 2

square x = x * x


areaOfACircle r = pi * (r * r)
