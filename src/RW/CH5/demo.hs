#!/usr/bin/env stack
-- stack script --resolver lts-11.7
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
    putStrLn "List version:"
    print $ take 10 [1..]
    putStrLn ""
    putStrLn "Conduit version:"
    print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList
