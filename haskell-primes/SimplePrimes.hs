-- This is the classic "divide by every prime up to the square root" version
-- of the primes list

module SimplePrimes where

primes :: [Integer]
primes = 2 : [x | x <- [3..], all (\p -> x `mod` p > 0) (factorsToTry x)]
  where
     factorsToTry x = takeWhile (\p -> p*p <= x) primes
