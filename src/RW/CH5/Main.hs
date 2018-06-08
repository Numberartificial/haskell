-- file ch05/Do.hs

module Main (main)

where
import RW.CH5.SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1)])
