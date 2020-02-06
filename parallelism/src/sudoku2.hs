import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe

-- stack ghc -O2 -- -o sudoku2 sudoku2.hs -rtsopts -threaded
-- ./sudoku2 sudoku17.1000.txt +RTS -N2 -s
-- stack ghc -O2 -- -o sudoku2 sudoku2.hs -rtsopts -threaded -eventlog
-- ./sudoku2 sudoku17.1000.txt +RTS -N2 -l
-- threadscope sudoku2.eventlog
-- Total time: 0.765s (x1.439)

main :: IO()
main = do 
    [f] <- getArgs
    file <- readFile f

    let puzzles = lines file 
        -- divide puzzle (almost) equally
        (as, bs) = splitAt (length puzzles `div` 2) puzzles

        solutions = 
            runEval $ do 
                -- force: WHNF -> NF (Contorl.Deepseq)
                -- rpar -> rpar -> rseq -> rseq design
                -- rpar: create spark
                as' <- rpar $ force $ map solve as
                bs' <- rpar $ force $ map solve bs
                -- rseq: wait for join
                rseq as'
                rseq bs'
                -- Append two lists
                return (as' ++ bs')
    
    print $ length $ filter isJust solutions