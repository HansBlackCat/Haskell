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
map f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:filter' f xs
    | otherwise = filter' f xs


