
pop :: [a] -> (a, [a])
pop ([x]) = (x, [])
pop (x:xs) = (x, xs)
pop _ = undefined

cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  line <- getLine
  let xs = map (read :: String -> Int) $ words line
  let (a, x1) = pop xs
  let (b, x2) = pop x1
  let (x, x3) = pop x2
  let (y, x4) = pop x3
  let (x1, x2) = (x, a - 1 - x)
  let (y1, y2) = (y, b - 1 - y)
  let area = maximum [calArea (x1, b), calArea (x2, b),calArea (a, y1), calArea (a, y2)]
  putStrLn $ show area
  cal (n - 1)
  where calArea (w, h) =  w * h

main :: IO ()
main = do
  a <- getLine
  let n = (read a) :: Int
  cal n
