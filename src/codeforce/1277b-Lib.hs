import System.Random

cal2n :: Int -> (Int, Int)
cal2n n = if n `mod` 2 == 0 then (i, 1 + m) else (n, 0)
  where (i, m) = cal2n (n `div` 2)

cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  m <- getLine
  line <- getLine
  let xs = map (read :: String -> Int) $ words line
  let pairs = map cal2n xs
  sorted <- qSortPair pairs
  let s = sum $ map snd $ sorted
  putStrLn $ show $ s
  cal (n - 1)

qSortPair :: [(Int, Int)] -> IO [(Int, Int)]
qSortPair [] = return []
qSortPair (x:[]) = return [x]
--qSortPair s@((i,m):xs) = do
qSortPair xs = do
        let len = length xs
        at <- randomRIO (0, len - 1)
        let (_, (rx:_)) = splitAt at xs
        let i = fst rx
        let l = filter (\(i1, _) -> i1 < i) xs
        let r = filter (\(i2, _) -> i2 > i) xs
        let mid = filter (\(i3, _) -> i3 == i) xs
        let m = maximum $ map snd $ mid
        ll <- qSortPair l
        rr <- qSortPair r
        return $ ll ++ [(i, m)] ++ rr

main :: IO ()
main = do
  n <- getLine
  cal $ (read :: String -> Int) n