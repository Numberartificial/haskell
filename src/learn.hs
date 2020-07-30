module Learn where

import           Data.List
import           System.Environment (getArgs)

median :: [Double] -> Double
median [] = 0
median xs =
  if oddInLength
    then middleValue
    else (middleValue + beforeMiddleValue) / 2
  where
    sortedList = sort xs
    oddInLength = 1 == mod (genericLength xs) 2
    middle = floor $ genericLength xs / 2
    middleValue = genericIndex sortedList middle
    beforeMiddleValue = genericIndex sortedList (middle - 1)

vowelIndices :: String -> [Integer]
vowelIndices s = map fst $ filter (\(_, letter) -> letter `elem` "aeiouAEIOU") $ zip [1 ..] s

word = "apple"

test :: IO ()
test = do
  values <- getArgs
  print . median $ map read values
