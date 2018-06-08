module InteractWith
where
import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

import Data.List (isPrefixOf)

dlts2 :: String -> [String]
dlts2 = map (head . tail . words) . filter ("#define DLT_" `isPrefixOf`) . lines

base = 65521

-- shiftL 函数实现逻辑左移， (.&.) 实现二进制位的并操作， (.|.) 实现二进制位的或操作， ord 函数则返回给定字符对应的编码值。
adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b []     = (b `shiftL` 16) .|. a

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = id

loop :: Int -> String -> Int
loop a (x:xs) = loop (a * 10 + digitToInt x) xs
loop a [] = a

asInt xs = loop 0 xs

oddList :: [Int] -> [Int]
oddList  = filter odd
