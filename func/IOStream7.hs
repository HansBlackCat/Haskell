import System.IO
import Data.Char

main = do
    contents <- readFile "haiku.txt"
    writeFile "haikuUP.txt" (map toUpper contents)
