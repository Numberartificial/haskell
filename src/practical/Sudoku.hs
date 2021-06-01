
import Control.Parallel.Strategies
import Control.Exception
import Control.Seq
import System.Environment
import Data.Maybe

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
    b <- rpar 

--`stack ghc -02 ./Sudoku.hs -rtsopts`
--it is important to compile with full
--optimization (-O2). The goal is to make the program run faster
-- Sudoku ./test.txt +RTS -s
--  ghc -O2 sudoku2.hs -rtsopts -threaded -eventlog
-- sudoku2 sudoku17.1000.txt +RTS -N2 -s -l
main::IO ()
main = do
    [f] <- getArgs 
    file <- readFile f
    let ls = lines f
        solutions = 
    print (length (lines file))