-- file : CH07/basicio.hs

module Basicio
where
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do  ineof <- hIsEOF inh
        if ineof
        then return ()
        else do inpStr <- hGetLine inh
                hPutStrLn outh (map toUpper inpStr)
                mainloop inh outh

lazyIO :: IO ()
lazyIO = do
  inh <- openFile "input.txt" ReadMode
  ouh <- openFile "output.txt" WriteMode
  content <- hGetContents inh
  let result = processData content
  hPutStr ouh result
  hClose inh
  hClose ouh

processData :: String -> String
processData = map toUpper

upIO :: IO ()
upIO = do
  ins <- readFile "input.txt"
  writeFile "output.txt" ins

main1 = do
  putStrLn "Greeting"
  inpStr <- getLine
  putStrLn $ "Welcome" ++ inpStr ++ "!"

main2 = do
  interact (unlines . filter (elem 'a') . lines)
