-- Generate Primes using "Classic" sieve, but with a limit
--
-- (c) 2006-2007 Melissa O'Neill.  Code may be used freely so long as
-- this copyright message is retained and changed versions of the file
-- are clearly marked.

module NaiveSieveLimit (primesToNth, primesToLimit) where

-- Here we use a wheel to generate all the number that are not multiples
-- of 2, 3, 5, and 7.  We use some hard-coded data for that.

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6
	:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

wheeler n (x:xs) = n : wheeler (n + x) xs

avoid2357 = wheeler 11 wheel

-- Often we want a finite list of primes, not an infinite one; so below
-- we have a revised version of the above code that produces finite lists.
-- The advantage of finite (but lazy) lists, even very long ones, is that
-- we don't have to keep track of every prime we encounter in the heap.

nThPrimeApprox :: Int -> Integer
nThPrimeApprox nth
    | n < 13    = 37
    | otherwise = round realApprox
    where
       n          = fromInteger (toInteger (nth))
       realApprox = n * (log n + log (log n) - 1 + 1.8 * log (log n) / log n)

primesToNth :: Int -> [Integer]
primesToNth n = take n (primesToLimit (nThPrimeApprox n))

primesToLimit :: Integer -> [Integer]
primesToLimit n
    | n < 11    = takeWhile (< n) [2,3,5,7]
    | otherwise = 2 : 3 : 5 : 7 : lsieve n avoid2357

lsieve :: Integer -> [Integer] -> [Integer]
lsieve limit ints = lsieve' ints where
    lsieve' ps@(p : xs)
        | p < sqrtlimit = p : lsieve' [x | x <- xs, x `mod` p > 0]
        | otherwise     = takeWhile (<= limit) ps
        where
            sqrtlimit = round (sqrt (fromInteger limit)) 

