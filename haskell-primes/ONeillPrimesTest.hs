-- Print out the nth prime, where n is the 1st argument

module Main where

import ONeillPrimes (primes)
import System (getArgs)

--- The largest prime we can safely calculate with Int arithmetic is the
--- 105,095,435th prime, which is 2,147,437,291 (prime closest to 
--- 2**31 - sqrt(2**31), but our nThPrimeApprox function overestimates,
--- so we have to limit choosing an the Int specializaton over the Integer
--- one at 103,945,550.
printNthPrime :: Int -> IO ()
printNthPrime n
    | n < 103945550 = print (n, (primes !! (n - 1)) :: Int) 
    | otherwise     = print (n, (primes !! (n - 1)) :: Integer) 

main = do
    args <- getArgs
    printNthPrime $ read $ head args
