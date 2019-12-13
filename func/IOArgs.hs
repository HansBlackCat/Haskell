import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <-getProgName
    putStrLn "The Arguments are: "
    mapM putStrLn args
    putStrLn "The Program name is: "
    putStrLn progName 
