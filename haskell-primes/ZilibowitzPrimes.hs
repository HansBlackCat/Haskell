-- Ruben Zilibowitz's primes algorithm

module ZilibowitzPrimes (primes) where

primes :: [Integer]
primes = primesFilter 1 [2..]

primesFilter :: Integer -> [Integer] -> [Integer]
primesFilter primorial (n:ns)
    | (gcd primorial n == 1) = n : (primesFilter (primorial*n) ns)
    | otherwise              = primesFilter primorial ns

