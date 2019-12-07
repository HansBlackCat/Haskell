-- This is the primes generator posted to Haskell Cafe by mailing list by
-- Yitzchak Gale

module GalePrimes (primes) where

-- Delete the elements of the first list from the second list,
-- where both lists are assumed sorted and without repetition.
deleteOrd :: Ord a => [a] -> [a] -> [a]
deleteOrd xs@(x:xs') ys@(y:ys')
  | x > y       = y : deleteOrd xs  ys'
  | x < y       =     deleteOrd xs' ys
  | otherwise   =     deleteOrd xs' ys'
deleteOrd _ ys = ys

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (deleteOrd [x+x,x+x+x..] xs)
sieve _      = []

primes :: [Integer]
primes = sieve [2..]

