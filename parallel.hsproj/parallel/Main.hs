import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f
  let puzzle = lines file
      solution = map solve puzzle
  print . length $ filter isJust solution  
