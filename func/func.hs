multiThree :: Int -> (Int -> (Int -> Int))
multiThree x y z = x*y*z

multiTwowithNine = multiThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100 

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] ->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

filp' :: (a -> b ->c) -> (b -> a -> c) 
filp' f = g
    where g x y = f y x
    
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:filter' f xs
    | otherwise = filter' f xs

largestDivisible :: Int -> Int -> Int
largestDivisible n m = head (filter p [n,n-1..])
    where p x = mod x m == 0

chain :: Int -> [Int]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n = n:chain (3*n + 1)

numLongChain :: Int -> Int
numLongChain n = length (filter isLong (map chain [1..n]))
    where isLong xs = length xs > 15

chainNumList :: Int -> [Int]
chainNumList 1 = [length (chain 1)]
chainNumList n = (length (chain n)) : chainNumList (n-1) 

listOfMulti = map (*) [0..]

numLongChain' :: Int
numLongChain' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc+x) 0 xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' a ys = foldr (\y acc -> if y==a then True else acc) False ys


