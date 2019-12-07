-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Interfaces
-- Copyright   :  (c) Andrew J. Bromage 2005
-- License     :  BSD-style
-- 
-- Maintainer  :  ajb@spamcop.net
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
-- This module provides interfaces and functions for working with
-- prime numbers.
--
-- The main algorithm used is to memo a set of small primes (all primes
-- less than 30030), and use this in conjunction with a wheel factorization
-- method to locate primes and factor numbers.
--
-- See: http://primes.utm.edu/glossary/page.php?sort=WheelFactorization
--
-----------------------------------------------------------------------------

module AtkinSieve (
    primesUpTo)
where

-- import Math.Interfaces
-- import Math.Util
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.IArray

isqrt i = fromInteger (truncate (sqrt (fromInteger (toInteger i))))

-- |Compute 'primesUpTo' a certain number.
primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < fromIntegral wheelModulus
        = takeWhile (<=n) . map fromIntegral $ smallPrimes
    | otherwise
        = (2:) . (3:) . squarefree $ atkinSieve n sn
        where
            sn = isqrt n

            squarefree [] = []
            squarefree xs'@(x:xs)
                | x <= sn
                    = (x:) . squarefree . mergeRemove xs
                        . takeWhile (<=n) $ [ k*x*x | k <- [1..] ]
                | otherwise
                    = xs'

            mergeRemove [] ys = []
            mergeRemove xs [] = xs
            mergeRemove xs'@(x:xs) ys'@(y:ys)
                = case compare x y of
                    EQ -> mergeRemove xs ys
                    LT -> x : mergeRemove xs ys'
                    GT -> mergeRemove xs' ys

{-# SPECIALIZE eratosthenesSieve :: Int -> [Int] #-}
{-# SPECIALIZE eratosthenesSieve :: Integer -> [Integer] #-}
eratosthenesSieve :: (Integral a) => a -> [a]
eratosthenesSieve n
  | n < 2     = []
  | otherwise = 2 : sieve [3,5..n]
  where
    isn = isqrt n

    sieve [] = []
    sieve ps'@(p:ps)
        | p <= isn  = p : sieve [ n | n <- ps, n `mod` p /= 0 ]
        | otherwise = ps'

verySmallPrimeCutoff :: Int
verySmallPrimeCutoff = 13

verySmallPrimes :: [Int]
verySmallPrimes = eratosthenesSieve verySmallPrimeCutoff

wheelModulus :: Int
wheelModulus = product verySmallPrimes

smallPrimes :: [Int]
smallPrimes = eratosthenesSieve (product verySmallPrimes)


-- There's probably a more efficient way to generate the code-based
-- binary search tree for the small primes rather than going through
-- an intermediate data structure.
--
-- By the way, see http://haskell.org/hawiki/RunTimeCompilation if you
-- don't understand this.

data SmallPrimeTree
    = SpLeaf0
    | SpLeaf1 Int
    | SpLeaf2 Int Int
    | SpLeaf3 Int Int Int
    | SpBranch Int SmallPrimeTree SmallPrimeTree

isSmallPrime :: Int -> Bool
isSmallPrime
    = test tree
    where 
        tree = toTree smallPrimes (length smallPrimes)

        toTree [] _ = SpLeaf0
        toTree [p1] _ = SpLeaf1 p1
        toTree [p1,p2] _ = SpLeaf2 p1 p2
        toTree [p1,p2,p3] _ = SpLeaf3 p1 p2 p3 
        toTree ps nps
            = let lps = nps `div` 2
                  ps1 = take lps ps
                  ps2 = drop lps ps
              in SpBranch (head ps2) (toTree ps1 lps) (toTree ps2 (nps-lps))

        test SpLeaf0
            = \p -> False
        test (SpLeaf1 p1)
            = \p -> p == p1
        test (SpLeaf2 p1 p2)
            = \p -> p == p1 || p == p2
        test (SpLeaf3 p1 p2 p3)
            = \p -> case compare p p2 of
                        LT -> p == p1
                        EQ -> True
                        GT -> p == p3
        test (SpBranch p' l r)
            -- We let-float by hand just in case the compiler doesn't
            -- do it for us.
            = let testl = test l
                  testr = test r
              in \p -> if p < p' then testl p else testr p


wheelSettings :: [Int]
wheelSettings = [ f | f <- [1..wheelModulus-1],
                    null [ p | p <- verySmallPrimes, f `mod` p == 0 ] ]

atkinSieve :: Integer -> Integer -> [Integer]
atkinSieve n sn
    = let
        asize = ((n + 11) `div` 12)
        a = runSTUArray (do
            a <- newArray (0,(asize)*4) False
            sieve a
            return a)
      in pullArray asize a
    where
        pullArray asize a
            = pullArray5 1 5
            where
                pullArray1 i p
                    | p > n = []
                    | a ! i     = p : pullArray5 (i+1) (p+4)
                    | otherwise = pullArray5 (i+1) (p+4)

                pullArray5 i p
                    | p > n = []
                    | a ! i     = p : pullArray7 (i+1) (p+2)
                    | otherwise = pullArray7 (i+1) (p+2)

                pullArray7 i p
                    | p > n = []
                    | a ! i     = p : pullArray11 (i+1) (p+4)
                    | otherwise = pullArray11 (i+1) (p+4)

                pullArray11 i p
                    | p > n = []
                    | a ! i     = p : pullArray1 (i+1) (p+2)
                    | otherwise = pullArray1 (i+1) (p+2)

        sieve a
            = sieve' [ (x,y) | x <- [1..sn], y <- [1..sn] ]
            where
                sieve' [] = return ()
                sieve' ((x,y):xys)
                    = do
                        let x2 = x*x
                            y2 = y*y

                        let ab1 = 4*x2 + y2
                        if ab1 <= n
                          then let ab1mod = ab1 `mod` 12
                            in case ab1mod of
                                1 -> do
                                    let k = (ab1 `div` 12)*4
                                    b <- readArray a k
                                    writeArray a k (not b)
                                5 -> do
                                    let k = (ab1 `div` 12)*4+1
                                    b <- readArray a k
                                    writeArray a k (not b)
                                _ -> return ()
                          else return ()

                        let ab2 = 3*x2 + y2
                        if ab2 <= n && ab2 `mod` 12 == 7
                          then do
                              let k = (ab2 `div` 12)*4+2
                              b <- readArray a k
                              writeArray a k (not b)
                          else return ()

                        let ab3 = 3*x2 - y2
                        if x > y && ab3 <= n && ab3 `mod` 12 == 11
                          then do
                              let k = (ab3 `div` 12)*4+3
                              b <- readArray a k
                              writeArray a k (not b)
                          else return ()

                        sieve' xys
