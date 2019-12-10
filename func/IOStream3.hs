import Data.Char

main = interact palind

palind :: String -> String
palind = unlines. map (\xs -> if xs == reverse xs then "palindrome" else "not palindrome"). lines

