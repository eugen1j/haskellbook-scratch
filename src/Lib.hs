module Lib where

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi, " ++ name ++ "!")


