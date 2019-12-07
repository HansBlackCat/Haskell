-- EratoS.hs: Eratosthenes' sieve
-- 2007-Feb-19 / TN
-- Test: t13

module NaurPrimes (primes, calcPrimes, primesToNth, primesToLimit) where

import Array
import List

-- Quick: All primes:

esPrimesAll :: [Integer]
esPrimesAll = filter ((==1) . (\a -> gcd a $ product [1..a-1])) [2..]

-- Wheeling:

-- Number of moduli:

nmod :: Int
nmod = 4

esSmallPrimes :: [Integer]
esLeastLargePrime :: Integer
(esSmallPrimes,(esLeastLargePrime:_)) = splitAt nmod esPrimesAll

ns :: [Int]
ns = map fromIntegral esSmallPrimes

nn :: Int
nn = product ns

-- Test table for wheelable:

wableTab :: Array Int Bool
wableTab = array (0,nn-1) [(i,gcd nn i == 1) | i<-[0..nn-1]]

-- Filter out numbers not wheelable:

wable :: Integral a => a -> Bool
wable x = wableTab ! (fromIntegral $ x`mod`fromIntegral nn)

wables :: Integral a => [a] -> [a]
wables = filter wable

-- Remainders with no common divisor with moduli:

rems :: [Int]
rems = wables [0..nn-1]

wBase :: Int
wBase = length rems

-- Table to convert a remainder to index:

remConv :: Array Int Int
remConv = array (0,nn-1) $ zip rems [(0::Int)..]

-- Convert to wheel representation:

toWheel :: Integral a => a -> a
toWheel x
  = let
      (q,r) = x`divMod`fromIntegral nn
      rInt :: Int
      rInt = fromIntegral r
    in
      fromIntegral wBase*q + fromIntegral ( remConv!rInt )

-- From wheel representation:

fromWheel :: Integral a => a -> a
fromWheel y
  = let
      (q,r) = y`divMod`fromIntegral wBase
      rInt :: Int
      rInt = fromIntegral r
    in
      fromIntegral nn*q + fromIntegral ( rems!!rInt )

-- Basic sequence of numbers k, k+p, ... with gcd N seq = 1:

red :: Integral a => a -> a -> [a]
red k p = map toWheel $ wables [k, k+p .. ]

-- Deltas:

deltas :: Integral a => a -> a -> [a]
deltas k p
  = let
      rs@(_:rs') = red k p
    in
      map (\(a,b) -> a-b) $ zip rs' rs

-- Wheel:

wheelNext :: Integral a => a -> a -> a
wheelNext k p = head $ wables [k, k+p .. ]

wheel :: Integral a => a -> a -> [a]
wheel k p
  = let
      k' = wheelNext k p
    in
      scanl (+) (toWheel k') (cycle $ take wBase $ deltas k' p)

-- Generic (Integral):

esCrossLow :: Integer -> Integer -> Integer
esCrossLow p from = p*p + max 0 (((from+p-1)`div`p - p) *p)

{-# INLINE esCrossGeneric #-}
esCrossGeneric :: (Integral a, Integral b) => b -> a -> [a]
esCrossGeneric from p
  = let
      k = fromIntegral $ esCrossLow (fromIntegral p) (fromIntegral from)
    in
      wheel k p

{-# SPECIALIZE esSieveByGeneric :: Int->Int->[[Int]]->[Int] #-}
esSieveByGeneric :: (Ix a, Integral a) => a -> a -> [[a]] -> [a]
esSieveByGeneric m n pss
  = map (fromWheel . fst) $ filter snd $ assocs $
      accumArray
        (\x -> \y -> False)
        True
        (toWheel m,toWheel n)
        [ (q,()) | ps <- pss, q <- ps ]

esSplit :: Ord a => a -> [a] -> ([a],[a])
esSplit limit ss
  = esSplit0 [] ss
    where
    esSplit0 ssLow ss@(s:ss')
      = if s < limit then
          esSplit0 (s:ssLow) ss'
        else
          (ssLow,ss)

esSieveByInt :: Int -> Int -> [[Int]] -> [Int]
esSieveByInt m n ps = esSieveByGeneric m n ps

intLimit :: Integral a => a
intLimit = fromIntegral (maxBound::Int)

esSectRawGeneric0 :: (Integral a, Ix a) => a -> a -> [[a]] -> [a]
esSectRawGeneric0 low high spss
  = if low <= intLimit && high <= intLimit then
      map fromIntegral $
        esSieveByInt (fromIntegral low) (fromIntegral high)
          (map (map fromIntegral) spss)
    else
      esSieveByGeneric low high spss

esSectRawGeneric :: (Integral a, Ix a) => a -> [[a]] -> [a] -> [[a]]
esSectRawGeneric low spss (p:ps)
  = let
      psq = p^2
      (spssLow,spssHigh) = unzip $ map (esSplit $ toWheel psq) spss
    in
      esSectRawGeneric0 low (wheelNext (psq-1) (negate 1)) spssLow :
        esSectRawGeneric psq (esCrossGeneric psq p:spssHigh) ps

esSectPrimesGeneric :: Integer -> [Integer]
esSectPrimesGeneric l
  = let
      (lowps,highps) = esSectPrimesAllSplit l
    in
      concat $ esSectRawGeneric l (map (esCrossGeneric l) lowps) highps

esSectPrimesFromGeneric :: Integer -> [Integer]
esSectPrimesFromGeneric m
  = let
      m' = (wheelNext (max 2 m) 1)
    in
      filter (\p -> m<=p && p < m') esSmallPrimes ++ esSectPrimesGeneric m'

esSectPrimesAllGeneric :: [Integer]
esSectPrimesAllGeneric = esSmallPrimes ++ esLeastLargePrime:esSectPrimesFromGeneric (esLeastLargePrime+1)

esSectPrimesAllSplit :: Integer -> ([Integer],[Integer])
esSectPrimesAllSplit l
  = primesSplit [] $ drop nmod esSectPrimesAllGeneric
    where
    primesSplit sps ps@(p:ps')
      = if l < p^2 then
          (sps,ps)
        else
          primesSplit (p:sps) ps'

esSectPrimesBetweenGeneric :: Integer -> Integer -> [Integer]
esSectPrimesBetweenGeneric m n = (takeWhile (<=n)) (esSectPrimesFromGeneric m)

calcPrimes :: () -> [Integer]
calcPrimes () =
  esSmallPrimes ++ esLeastLargePrime:esSectPrimesFromGeneric (esLeastLargePrime+1)

primes :: [Integer]
primes = calcPrimes ()

primesToNth :: Int -> [Integer]
primesToNth n = take n (calcPrimes ())

primesToLimit :: Integer -> [Integer]
primesToLimit limit = takeWhile (< limit) (calcPrimes ())
  