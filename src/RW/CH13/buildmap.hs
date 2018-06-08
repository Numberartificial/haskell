-- files : CH13/buildmap.hs

module BMap
where

import qualified Data.Map as Map

-- Functions to generate a Map that represents an association list
-- as a map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

-- Create a map representation of 'al' by converting the association 
-- list using Map.fromList
mapFromAL =
    Map.fromList al

-- Create a map representation of 'al' by doing a fold
mapFold = 
    foldl (\map (k, v) -> Map.insert k v map) Map.empty al

-- Manually create a map with the elements of 'al' in it
mapManual =
    Map.insert 2 "two" .
    Map.insert 4 "four" .
    Map.insert 1 "one" .
    Map.insert 3 "three" $ Map.empty

data FuncRec =
  FuncRec { name :: String,
            calc :: Int -> Int,
            nameCalc :: Int -> (String, Int)
          }

mkFuncRec :: String -> (Int -> Int) -> FuncRec
mkFuncRec name calc =
  FuncRec {name = name,
          calc = calc,
          nameCalc = \i -> (name, calc i)}

plus5 = mkFuncRec "+5" (+5)


