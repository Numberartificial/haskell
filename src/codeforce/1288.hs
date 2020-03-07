
iter :: [Integer] -> [Integer]
iter xs = tail $ scanl (+) 0 xs

cal :: Int -> Int -> [Integer]
cal n 1 =  take n $ repeat 1
cal n m =
  let ls = cal n 1
      itf = foldl (.) (id) $ take (m - 1) $ repeat (iter)
  in
      itf ls
--   let a = cal n (m - 1)
--       b = cal (n - 1) m
--       v = foldr (+) 0 a
--   in
--       b ++ [v]



calSum :: Int -> Int -> Integer -> Integer
calSum n m d =
  let
    xs = map (`mod` d) $ cal n m
    ss = reverse $ tail $ scanl (+) 0 xs
  in
    (`mod` d) . sum $ zipWith ((*)) xs ss


main :: IO ()
main = do
  line <- getLine
  let (n:m:[]) = map (read :: String -> Int) $ words line
  putStrLn $ show $ calSum n m 1000000007