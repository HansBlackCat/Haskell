import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

-- stack ghc -- -O2 -o sudoku1 sudoku1.hs -rtsopts
-- ./sudoku1 sudoku17.1000.txt
-- Total time: 1.101s

main :: IO ()
main = do
    -- grab shell's command line args 
    [f] <- getArgs 
    -- read contents
    file <- readFile f 
    
    -- split files into lines
    let puzzles = lines file
    -- solve(Sudoku)
        soultions = map solve puzzles
    
    print $ length $ filter isJust soultions



