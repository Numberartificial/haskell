-- |
{-# LANGUAGE NoImplicitPrelude #-}

module WordNumber where

import Data.List (concat, head, intercalate, intersperse, map)
import Prelude (Char, Int, String, (.), read, show, undefined)

readCons         :: Char   -> Int
readCons = (read :: String -> Int) . (: [])

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "zero"

digits :: Int -> [Int]
digits = map readCons . show

wordNumber :: Int -> String
--wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
wordNumber     = intercalate "-" . map digitToWord . digits
