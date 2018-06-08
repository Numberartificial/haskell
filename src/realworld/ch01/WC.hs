-- file :  ch01/WC.hs
module WC
where
import Control.Monad

main :: IO ()
main = do
  putStr $ "input filename"
  filePath <- getLine
  content <- readFile filePath --"quue.txt"
  putStrLn $ wordCount content
  putStrLn $ charCount content
   where
  wordCount input = show (length ( lines input >>= words)) ++ "\n"
  charCount input = show (length . join $ (lines input >>= words))

mydrop :: Integral a => a -> [b] -> [b]
mydrop n xs = if n <= 0 || null xs
  then xs
  else mydrop (n - 1) (tail xs)
