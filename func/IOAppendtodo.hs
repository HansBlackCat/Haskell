import System.IO
import Data.Char

main = do
    todo <- getLine
    appendFile "todo.txt" (todo ++ "\n")
