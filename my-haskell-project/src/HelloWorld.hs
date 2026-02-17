module HelloWorld where

sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

-- Optional: A main function if you want to run this file directly
main :: IO ()
main = putStrLn (sayHello "World")