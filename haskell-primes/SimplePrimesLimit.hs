-- This is the classic "divide by every prime up to the square root" version
-- of the primes list, but with a limit

module SimplePrimesLimit (primesToNth, primesToLimit) where

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

isqrt i = fromInteger (truncate (sqrt (fromInteger (toInteger i))))

{-
primesToLimit :: Integer -> [Integer]
primesToLimit limit = takeWhile (<= limit) primes where
    limitsqrt = isqrt limit
    primes' = primesToLimit limitsqrt
    primes = 2 : ((all (> 1) primes') `seq`  -- make sure primes' gets an end
              [x | x <- [3,5..], all (\p -> x `mod` p > 0) (factorsToTry x)])
      where    
        factorsToTry x = takeWhile (\p -> p*p <= x) primes'
-}

primesToLimit :: Integer -> [Integer]
primesToLimit limit = takeWhile (<= limit) primes
  where
      limitsqrt = isqrt limit
      primes' = if (limitsqrt < 5) then [2,3] else primesToLimit limitsqrt
      c       = (last primes') + 2
      primes  = primes' ++ more
      more    = [x | x <- [c,c+2..], all (\p -> x `mod` p > 0) (factorsToTry x)]
        where    
          factorsToTry x = takeWhile (\p -> p*p <= x) primes'
