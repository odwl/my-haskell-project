module Main where

import HelloWorld (sayHello)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    let result = sayHello "User"
    let expected = "Hello, User!"
    
    if result == expected
        then do
            putStrLn "Test Passed!"
            exitSuccess
        else do
            putStrLn $ "Test Failed: expected " ++ expected ++ " but got " ++ result
            exitFailure