-- This is the primes generator posted to Haskell Cafe by mailing list by
-- Yitzchak Gale

module GalePrimes2 (primes) where

crossOff :: Ord a => [a] -> [Either a a] -> [Either a a]
crossOff xs@(x:xs') ys@(y:ys')
    | x < y'    = crossOff xs' ys
    | x > y'    = y : crossOff xs  ys'
    | otherwise = Left y' : crossOff xs' ys'
    where y' = fromEither y
crossOff _ y = y

sieve (Right x:xs) = Right x : sieve (crossOff [x+x,x+x+x..] xs)
sieve (Left  x:xs) = Left  x : sieve xs

primes :: [Integer]
primes = catRights $ sieve $ map Right [2..]

fromEither = either id id

catRights :: [Either a b] -> [b]
catRights (Right x:xs) = x : catRights xs
catRights (      _:xs) =     catRights xs
catRights _            = []
