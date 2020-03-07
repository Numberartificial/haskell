
cal2n :: Int -> (Int, Int)
cal2n n = if n `mod` 2 == 0 then (i, 1 + m) else (n, 0)
  where (i, m) = cal2n (n `div` 2)

cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  m <- getLine
  line <- getLine
  let xs = map (read :: String -> Int) $ words line
  let s = sum $ map snd $ qSortPair $ map cal2n xs
  putStrLn $ show $ s
  cal (n - 1)

qSortPair :: [(Int, Int)] -> [(Int, Int)]
qSortPair [] = []
qSortPair s@((i,m):xs) = (qSortPair ls) ++ [(i, mid)] ++ (qSortPair rs)
  where ls = filter (\(i1, m1) -> i1 < i) xs
        rs = filter (\(i2, m2) -> i2 > i) xs
        mid = maximum $ map snd $ filter (\(i3, m3) -> i3 == i) s


main :: IO ()
main = do
  n <- getLine
  cal $ (read :: String -> Int) n