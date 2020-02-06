import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

-- stack ghc --  -O2 -o sudoku4 sudoku4.hs -rtsopts -threaded -eventlog
-- ./sudoku4 sudoku17.1000.txt +RTS -N2 -s
-- Total time: 0.621s (x1.770)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f 

    let puzzles = lines file 
        -- sudoku1 -> map solve puzzles
        -- sudoku3 -> runEval $ parMap solve puzzles 
        solutions = runEval (parMap solve puzzles)
    
    -- evalutate first, like seq command in ghci
    evaluate $ length puzzles
    print $ length $ filter isJust solutions

-- functor
-- create one spark for each elements in list
parMap :: (a->b) -> [a] -> Eval [b]
parMap f [] = return [] 
parMap f (a:as) = do 
    b <- rpar $ f a
    bs <- parMap f as
    return (b:bs)