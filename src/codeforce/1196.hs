
cal :: Int -> IO ()
cal 0 = return ()
cal n = do
  line <- getLine
  let (a:b:c:[]) = map (read :: String -> Integer) $ words line
  putStrLn $ show $ (a + b + c) `div` 2
  cal (n - 1)

main :: IO ()
main = do
  n <- getLine
  cal $ read n