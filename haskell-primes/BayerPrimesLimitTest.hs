-- Print out the nth prime, where n is the 1st argument

module Main where

import BayerPrimes (primesToNth)
import System (getArgs)

--- The largest prime we can safely calculate with Int arithmetic is the
--- 105,095,435th prime, which is 2,147,437,291 (prime closest to 
--- 2**31 - sqrt(2**31).
printNthPrime :: Int -> IO ()
printNthPrime n
    | n < 105095434 = print (n, last(primesToNth n) :: Int) 
    | otherwise     = print (n, last(primesToNth n) :: Integer) 

main = do
    args <- getArgs
    printNthPrime $ read $ head args
