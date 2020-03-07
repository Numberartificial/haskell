{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
--https://ghc.haskell.org/trac/haskell-prime/wiki/FlexibleInstances

class This a where
  doThis :: a -> Bool

instance This Int where
  doThis a = a > 42

newtype G = G Int deriving (Eq, Show, This)

ordered :: Int -> [Int] -> (Bool, Int, Int, Int)
ordered 1 (x:[]) = (True, 1, x, x)
ordered n xs     =
              if (bl && br) then
                   if (maxl <= minr) then (True, n, minl, maxr)
                   else (False, max sl sr, minl, maxl)
              else
                  (False, max sl sr, minl, maxl)
              where
                nh = n `div` 2
                xl = take nh xs
                xr = drop nh xs
                (bl, sl, minl, maxl) = ordered nh xl
                (br, sr, minr, maxr) = ordered nh xr



cal :: Int -> [Int] -> IO ()
cal n xs = do
  let (_, a, _, _) = ordered n xs
  putStrLn $ show a

main :: IO ()
main = do
  a <- getLine
  word <- getLine
  let
    n = (read a):: Int
    xs = map (read :: String -> Int) $ words word
  cal n xs
