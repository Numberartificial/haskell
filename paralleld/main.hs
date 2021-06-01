-- depedence: parallel
import Control.Parallel.Strategies

--`stack ghc -02 ./Sudoku.hs -rtsopts`
--it is important to compile with full
--optimization (-O2). The goal is to make the program run faster
-- Sudoku ./test.txt +RTS -s
--  ghc -O2 sudoku2.hs -rtsopts -threaded -eventlog
-- sudoku2 sudoku17.1000.txt +RTS -N2 -s -l


-- NOTE: WHNF{weak head normal form}/ NF{normal form}
-- :info Eval
-- :type runEval/ rpar/ rseq

f:: a -> ()
f _ = ()

-- rpar/rpar mode, return happens immediately
pp :: ((), ())
pp = do
    runEval $ do
        a <- rpar (f 1)
        b <- rpar (f 1)
        return (a, b)

-- rpar/rseq mod, return waits for the completion of rseq evaluation
ps :: ((), ())
ps = do
    runEval $ do
        a <- rpar (f 1)
        b <- rseq (f 1)
        return (a, b)

-- rpar/rseq/rseq mod, return happens when both completed.
pss :: ((), ())
pss = do
    runEval $ do
        a <- rpar (f 1)
        b <- rseq (f 1)
        rseq a
        return (a, b)

main :: IO ()
main = do
    return $ fst pss