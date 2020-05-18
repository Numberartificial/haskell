module Lib
    ( someFunc
    ) where
    
import Text.CSV

someFunc :: IO ()
someFunc = parseC

median :: [Double] -> Double
median xs = head xs

parseC = do
    e <- parseCSVFromFile "data/content/odps.ypp_recommend.anchor_profile.csv"
    case e of
      Left msg -> print msg
      Right csvF -> do
          let columns = map head $ tail csvF
          let formatStrs = map (\c -> "    cast (" ++ c ++" as String), \n") columns
          writeFile "data/anchor_profile_f1.sql" $ concat formatStrs 
          let f2 = map (\c -> "    " ++ c ++"       String  COMMENT ''  , \n") columns
          writeFile "data/anchor_profile_f2.sql" $ concat f2


   
