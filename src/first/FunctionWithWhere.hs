module FunctionWithWhere where

main :: IO ()
main = do
  putStrLn "this"
  putStrLn test
  where test = concat ["is ","a", " new world!"]

