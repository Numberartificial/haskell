pair :: [Int] -> Int
pair [] = 0
pair (a:b:xs) = abs (a - b) + pair xs

cal :: Int -> IO ()
cal n = do
  line <- getLine
  let xs = take n $ map (read :: String -> Int) $ words line
  let ordered = qSort xs
  putStrLn $ show $ pair ordered

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (p:xs) = (qSort lesser) ++ [p] ++ (qSort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

main :: IO ()
main = do
  a <- getLine
  let n = (read :: String -> Int) a
  cal n