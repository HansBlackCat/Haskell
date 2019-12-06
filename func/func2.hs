maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) [] 

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*) 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []  

last' :: [a] -> a
last' = foldl1 (\ _ x -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True

--sqrtSum :: Integer
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))

-- sum (filter (>10) map (*2) [2..10]) == sum $ filter (>10) (map (*2) [2..10])
dollarSignTest :: [Double]
dollarSignTest = map ($3) [(4+), (10*), (^2), sqrt]

funcComTest :: (Num a, Enum a) => [a]
funcComTest = map (negate . abs) [-4,-3..4]

shortenSum   = sum (replicate 5 (max 6.7 8.9))
shortenSum'  = (sum . replicate 5) (max 6.7 8.9)
shortenSum'' = sum . replicate 5 $ max 6.7 8.9

fn x = ceiling (negate (tan (cos (max 50 x))))
fn'  = ceiling . negate . tan . cos . max 50 


