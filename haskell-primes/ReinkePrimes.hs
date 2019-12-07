-- This is the primes generator posted to Haskell Cafe by mailing list by
-- Claus Reinke

module ReinkePrimes (primes) where

primes :: [Integer]
primes = 2 : filter (>2) (foldr sieve id primes [0..])

-- start sieve for p at position p*p, zeroing position off=(p-1)*(p-1)
sieve p f xs@(off:_) = 0: tail (applyAt (p*p-off) (f . sieve' p) xs) 
-- zero positions that are multiples of p
sieve' p = applyAt p (\(_:b)->sieve' p (0:b))

-- like splitAt, followed by (++), where only suffix is operated on
-- infinite lists, non-negative offset
applyAt 0 f xs     = f xs
applyAt n f (x:xs) = x:applyAt (n-1) f xs
