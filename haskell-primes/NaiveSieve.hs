-- Naive Sieve

module NaiveSieve (primes, sieve) where

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
