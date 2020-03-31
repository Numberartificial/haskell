
type P = (Int, Int)

cal2n :: P -> P
cal2n (c, n) = if n `mod` 2 == 1 then (c, n)
            else
              cal2n (c + 1, n `div` 2)

calSum :: [Int] -> Int
calSum xs =
   let pairs = map (\n -> cal2n (0, n)) xs
       sorted = qSortPair pairs
   in  sum $ map fst $ sorted

cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  m <- getLine
  line <- getLine
  let xs = map (read :: String -> Int) $ words line
  putStrLn $ show $ calSum xs
  cal (n - 1)

ran :: [P] -> P
ran s@(x:xs) =
           let
             len = length xs
             at = if odd len then
                     if odd (fst x + snd x) then div len 2 + 1 else div len 2
                  else
                     if even (fst x + snd x + len) then 2 else 1
             (_, (rx:_)) = splitAt at s
           in
             rx

qSortPair :: [P] -> [P]
qSortPair [] = []
qSortPair (x:[]) = [x]
--qSortPair s@((i,m):xs) = do
qSortPair xs =
        let rx = ran xs
            i = snd rx
            l = filter (\(_, i1) -> i1 < i) xs
            r = filter (\(_, i2) -> i2 > i) xs
            mid = filter (\(_, i3) -> i3 == i) xs
            m = maximum $ map fst $ mid
            ll = qSortPair l
            rr = qSortPair r
        in
            ll ++ [(m, i)] ++ rr

main :: IO ()
main = do
  n <- getLine
  cal $ (read :: String -> Int) n