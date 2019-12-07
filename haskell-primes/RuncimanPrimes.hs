-- Code from Colin Runciman's JFP paper, _Lazy Wheel Sieves and Spirals of
-- Primes_.  (Changed from Int to Integer to match other code)

module RuncimanPrimes (primes) where
data Wheel = Wheel Integer [Integer] [Integer]

wheels =
  Wheel 1 [1] [] :
    zipWith3 nextSize wheels primes squares

nextSize (Wheel s ms ns) p q =
    Wheel (s*p) ms' ns'
    where
      (xs,ns') = span (<=q) (foldr (turn 0) (roll (p-1) s) ns)
      ms' = foldr (turn 0) xs ms
      roll 0 _ = []
      roll t o = 
        foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
      turn o n rs =
        let n' = o+n in [n' | mod n' p > 0] ++ rs

primes :: [Integer]
primes = spiral wheels primes squares

squares = [p*p | p <- primes]

spiral (Wheel s ms ns : ws) ps qs =
  foldr (turn 0) (roll s) ns
  where
    roll o =
      foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
    turn o n rs =
      let n' = o+n in
      if n' == 2 || n' < head qs then n' : rs
      else dropWhile (<n') (spiral ws (tail ps) (tail qs))

