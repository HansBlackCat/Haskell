import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

-- stack ghc --  -O2 -o sudoku3 sudoku3.hs -rtsopts -threaded -eventlog
-- ./sudoku3 sudoku17.1000.txt +RTS -N2 -s
-- Total time: 0.622s (x1.770)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f 

    let puzzles = lines file 
        -- sudoku1 -> map solve puzzles
        -- sudoku3 -> runEval $ parMap solve puzzles 
        solutions = runEval (parMap solve puzzles)
    
    print (length (filter isJust solutions))

-- functor
-- create one spark for each elements in list
parMap :: (a->b) -> [a] -> Eval [b]
parMap f [] = return [] 
parMap f (a:as) = do 
    b <- rpar (f a)
    bs <- parMap f as
    return (b:bs)

