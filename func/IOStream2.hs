import Data.Char

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines. filter (\ipt -> length ipt < 10). lines


