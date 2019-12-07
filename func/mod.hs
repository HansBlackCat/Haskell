import Data.List
import Data.Char
import qualified Data.Map as Map
import Geometry

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)

makeOrd :: [Char] -> [Int]
makeOrd = map (ord)  

encode' :: Int -> String -> String
encode' n str = map (\a -> chr $ ord a + n) str
encode'' n str = map (chr . (+ n) . ord) str

decode' :: Int -> String -> String
decode' n str = map (chr . (subtract n) . ord) str

digitSum :: Int -> Int
digitSum = sum . map (digitToInt) . show

firstToN :: Int -> Maybe Int
firstToN n = find (\x -> digitSum x == n) [1..]

dicList :: [([Char], Int)]
dicList =
    [("A", 11)
    ,("B", 12)
    ,("C", 32)
    ,("D", 54)
    ,("E", 98)
    ,("F", 30)]

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ [] = Nothing
findKey' k ((key, value):xs)
    | key == k = Just value
    | otherwise = findKey' k xs

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing

newBook :: [([Char],[Char])]
newBook =
    [("St", "334-9934")
    ,("Xm", "221-3942")
    ,("Mi", "093-1853")]

stringToInt :: String -> [Int]
stringToInt = map digitToInt . filter isDigit 
