-- Generate Primes using "Classic" sieve, but using "the wheel"
--
-- (c) 2006-2007 Melissa O'Neill.  Code may be used freely so long as
-- this copyright message is retained and changed versions of the file
-- are clearly marked.

module NaiveSieveWheel (primes) where

-- Here we use a wheel to generate all the number that are not multiples
-- of 2, 3, 5, and 7.  We use some hard-coded data for that.

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6
        :2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

wheeler n (x:xs) = n : wheeler (n + x) xs

avoid2357 = wheeler 11 wheel

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : sieve avoid2357

sieve :: [Integer] -> [Integer]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
