import System.IO

main = do
    contents <- readFile "haiku.txt"
    putStr contents
