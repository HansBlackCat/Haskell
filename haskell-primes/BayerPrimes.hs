module BayerPrimes (venturi,primes,primesToNth,primesToLimit) where

-- Code for venturi :: Ord a => [[a]] -> [a]

{-# SPECIALIZE merge :: [Int] -> [Int] -> [Int] -> [Int] #-}
{-# SPECIALIZE merge :: [Integer] -> [Integer] -> [Integer] -> [Integer] #-}
merge :: Ord a => [a] -> [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) zs@(z:zt)
    | x <= y = if x <= z
        then x : (merge xt ys zs)
        else z : (merge xs ys zt)
    | otherwise = if y <= z
        then y : (merge xs yt zs)
        else z : (merge xs ys zt)
merge _ _ _ = undefined

data List a = A a (List a) | B [a]

{-# SPECIALIZE mergeA :: List Int -> List Int -> List Int -> List Int #-}
{-# SPECIALIZE mergeA :: List Integer -> List Integer -> List Integer -> List Integer #-}
mergeA :: Ord a => List a -> List a -> List a -> List a
mergeA (A x xt) ys zs = A x (mergeA xt ys zs)
mergeA (B xs)   ys zs = mergeB xs ys zs

{-# SPECIALIZE mergeB :: [Int] -> List Int -> List Int -> List Int #-}
{-# SPECIALIZE mergeB :: [Integer] -> List Integer -> List Integer -> List Integer #-}
mergeB :: Ord a => [a] -> List a -> List a -> List a
mergeB xs@(x:xt) ys@(A y yt) zs = case compare x y of
    LT -> A x (mergeB xt ys zs)
    EQ -> A x (mergeB xt yt zs)
    GT -> A y (mergeB xs yt zs)
mergeB xs (B ys) zs = mergeC xs ys zs
mergeB _ _ _ = undefined

{-# SPECIALIZE mergeC :: [Int] -> [Int] -> List Int -> List Int #-}
{-# SPECIALIZE mergeC :: [Integer] -> [Integer] -> List Integer -> List Integer #-}
mergeC :: Ord a => [a] -> [a] -> List a -> List a
mergeC xs@(x:xt) ys@(y:yt) zs@(A z zt)
    | x < y = if x < z
        then A x (mergeC xt ys zs)
        else A z (mergeC xs ys zt)
    | otherwise = if y < z
        then A y (mergeC xs yt zs)
        else A z (mergeC xs ys zt)
mergeC xs ys (B zs) = B $ merge xs ys zs
mergeC _ _ _ = undefined

{-# SPECIALIZE root :: List Int -> [List Int] -> [Int] #-}
{-# SPECIALIZE root :: List Integer -> [List Integer] -> [Integer] #-}
root :: Ord a => List a -> [List a] -> [a]
root (A x xt) yss       = x : (root xt yss)
root (B xs) (ys:zs:yst) = root (mergeB xs ys zs) yst
root _ _ = undefined

{-# SPECIALIZE wrap :: [Int] -> List Int #-}
{-# SPECIALIZE wrap :: [Integer] -> List Integer #-}
wrap [] = B []
wrap (x:xt) = A x $ B xt

{-# SPECIALIZE triple :: [List Int] -> [List Int] #-}
{-# SPECIALIZE triple :: [List Integer] -> [List Integer] #-}
triple :: Ord a => [List a] -> [List a]
triple (x:y:z:xs) = mergeA x y z : (triple xs)
triple _ = undefined

{-# SPECIALIZE group :: [List Int] -> [List Int] #-}
{-# SPECIALIZE group :: [List Integer] -> [List Integer] #-}
group :: Ord a => [List a] -> [List a]
group (x:y:xt) = x : y : (group $ triple xt)
group _ = undefined

{-# SPECIALIZE venturi :: [[Int]] -> [Int] #-}
{-# SPECIALIZE venturi :: [[Integer]] -> [Integer] #-}
venturi :: Ord a => [[a]] -> [a]
venturi (x:xt) = root (wrap x) $ group $ map wrap xt
venturi _ = undefined

-- Code for primes :: Integral a => [a]

{-# SPECIALIZE diff  :: [Int] -> [Int] -> [Int] #-}
{-# SPECIALIZE diff  :: [Integer] -> [Integer] -> [Integer] #-}
diff  :: Ord a => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : (diff  xt ys)
    EQ ->     (diff  xt yt)
    GT ->     (diff  xs yt)
diff _ _ = undefined

{-# SPECIALIZE trim :: Int -> [Int] -> [Int] #-}
{-# SPECIALIZE trim :: Integer -> [Integer] -> [Integer] #-}
trim :: Integral a => a -> [a] -> [a]
trim p = let f m x = mod x m /= 0 in filter (f p)

{-# SPECIALIZE seed :: [Int] #-}
{-# SPECIALIZE seed :: [Integer] #-}
seed :: Integral a => [a]
seed = [2,3,5,7,11,13,17]

{-# SPECIALIZE wheel :: [Int] #-}
{-# SPECIALIZE wheel :: [Integer] #-}
wheel :: Integral a => [a]
wheel = drop 1 [ m*j + k | j <- [0..], k <- ws ]
    where m  = foldr1 (*) seed
          ws = foldr trim [1..m] seed

{-# SPECIALIZE multiples :: [Int] -> [[Int]] #-}
{-# SPECIALIZE multiples :: [Integer] -> [[Integer]] #-}
multiples :: Integral a => [a] -> [[a]]
multiples ws = map fst $ tail $ iterate g ([], ws)
    where g (_,ps@(p:pt)) = ([ m*p | m <- ps ], trim p pt)
          g _ = undefined

-- Sometimes, memoization isn't your friend.  Maybe you don't actually want
-- to remember all the primes for the duration of your program and doing so
-- is just wasted space.  For that situation, we provide calcPrimes which
-- calculates the infinite list of primes from scratch.

{-# SPECIALIZE calcPrimes :: () -> [Int] #-}
{-# SPECIALIZE calcPrimes :: () -> [Integer] #-}
calcPrimes :: Integral a => () -> [a]
calcPrimes () = seed ++ (diff wheel $ venturi $ multiples wheel)

{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}
primes :: Integral a => [a]
primes = calcPrimes ()

{-# SPECIALIZE primesToNth :: Int -> [Integer] #-}
{-# SPECIALIZE primesToNth :: Int -> [Int] #-}
primesToNth :: Integral a => Int -> [a]
primesToNth n = take n (calcPrimes ())

{-# SPECIALIZE primesToLimit :: Integer -> [Integer] #-}
{-# SPECIALIZE primesToLimit :: Int -> [Int] #-}
primesToLimit :: Integral a => a -> [a]
primesToLimit limit = takeWhile (< limit) (calcPrimes ())
