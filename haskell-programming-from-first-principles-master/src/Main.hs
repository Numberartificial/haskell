module Main where

import Prelude hiding (map)

greet :: String -> String
greet "xxx" = "who is xxx?"
greet name  = "Hello " ++ name ++ " !@@@"

mini :: Int -> Int -> Int
mini 0 0                   = 0
mini x y | x > y && x == y = x
         | otherwise       = y

mio :: String -> IO String
mio s = do x   <- getLine
           abc <- getLine
           return $ s ++ x ++ abc

main :: IO ()
main = do putStrLn "Haskell Programming From First Principles"
          putStrLn $ greet "Michael"
