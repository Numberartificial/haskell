#!/usr/bin/env stack
{-# LANGUAGE NoMonomorphismRestriction #-}
-- stack --resolver lts-13.7 script

module DetermineTheType where

example = 1

main :: IO ()
main = do
  putStrLn "hello World"
  print example


