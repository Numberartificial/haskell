module Lib
  ( someFunc
  , testd
  , median
  , test
  , vowelIndices
  , getColumnInCSV
  ) where

import           Text.CSV
import  Data.List
import Learn
import DATAD.Headf

someFunc :: IO ()
someFunc = parseC

parseC = do
  e <- parseCSVFromFile "data/content/odps.ypp_recommend.anchor_profile.csv"
  case e of
    Left msg -> print msg
    Right csvF -> do
      let columns = map head $ tail csvF
      let formatStrs = map (\c -> "    cast (" ++ c ++ " as String), \n") columns
      writeFile "data/anchor_profile_f1.sql" $ concat formatStrs
      let f2 = map (\c -> "    " ++ c ++ "       String  COMMENT ''  , \n") columns
      writeFile "data/anchor_profile_f2.sql" $ concat f2

testd :: IO (Either String Integer)
testd = parseC2

parseC2 = do
  csv <- parseCSVFromFile "data/content/odps.ypp_recommend.anchor_profile.csv"
  return $ either (\error -> Left "Problem Reading File")
    (`getColumnInCSV` "字段类型") csv
