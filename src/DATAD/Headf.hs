module DATAD.Headf where

import Data.List
import Data.Either
import Text.CSV

getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv columnName =
  case lookupResponse of
    Nothing -> Left "The column does not exist in this CSV file"
    Just x -> Right (fromIntegral x)
  -- This line looks to see if column is in our CSV file
  where
    lookupResponse = elemIndex columnName (head csv)
