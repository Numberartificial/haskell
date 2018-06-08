-- files: ch09/FoldDir.hs

module FoldDir
where

data Iterate seed = Done {unwrap :: seed}
                   | Skip {unwrap :: seed}
                   | Continue {unwrap :: seed}
                   deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
