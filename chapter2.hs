module Chapter2 where
-- First, w decalre the name of our module so
-- it can be imported by name in a project

x = 10 
    * 5 + y

myResult = x * 5

y = 10 

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

printInc2 n = let plusTwo = n + 2 
                in print plusTwo

printInc n = print plusTwo
  where plusTwo = n + 2

mult1    = x * y
 where x = 5
       y = 6
