-- A primes algorithm as posted to Haskell Cafe by "oleg"
-- http://www.haskell.org/pipermail/haskell-cafe/2007-February/022437.html

module OlegPrimes where

-- repl_every_n n l replaces every (n+1)-th element in a list (_:l)
-- with False
repl_every_n :: Int -> [Bool] -> [Bool]
repl_every_n n l = repl_every_n' n l
 where repl_every_n' 0 (_:t) = False: repl_every_n n t
       repl_every_n' i (h:t) = h:     repl_every_n' (pred i) t

primes = 2:(loop 3 (repeat True))
 where loop n (False:t) = loop (succ (succ n)) t
       loop n (_:t)  = n:(loop (succ (succ n)) (repl_every_n (pred n) t))
