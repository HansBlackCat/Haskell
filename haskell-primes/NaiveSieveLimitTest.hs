-- Print out the nth prime, where n is the 1st argument

module Main where

import NaiveSieveLimit (primesToNth)
import System (getArgs)

printNthPrime :: Int -> IO ()
printNthPrime n = print (n, last(primesToNth n)) 

main = do
    args <- getArgs
    printNthPrime $ read $ head args
