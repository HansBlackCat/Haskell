import Data.List
import Data.Char

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)

makeOrd :: [Char] -> [Int]
makeOrd = map (ord)  
