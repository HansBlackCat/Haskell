-- Print out the nth prime, where n is the 1st argument

module Main where

import GalePrimes2 (primes)
import System (getArgs)

printNthPrime :: Int -> IO ()
printNthPrime n = print (n, primes !! (n - 1)) 

main = do
    args <- getArgs
    printNthPrime $ read $ head args
