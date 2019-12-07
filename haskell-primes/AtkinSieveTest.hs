-- Print out the nth prime, where n is the 1st argument

module Main where

import AtkinSieve (primesUpTo)
import System (getArgs)

printNthPrime :: Int -> IO ()
printNthPrime n = print (n, last (primesToNth n)) 

main = do
    args <- getArgs
    printNthPrime $ read $ head args

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
primesToLimit n = primesUpTo n
