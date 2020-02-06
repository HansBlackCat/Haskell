import Control.Parallel
import Control.Parallel.Strategies (rpar, Strategy, using)
import Text.Printf
import System.Environment

-- ./strat +RTS -N2 -l
-- Total time: 0.952s 
-- fib_normal Total time: 1.514s

-- type Strategy a = a -> Eval a
-- Slightly different from rpar/rpar pattern
parPair :: Strategy (a,b)
parPair (a, b) = do 
  a' <- rpar a
  b' <- rpar b 
  return (a', b')

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- using :: a -> Strategy a -> a
-- using x s = runEval $ s x
-- runEval $ parPair (fib 35, fib 36) 
-- == (fib35, fib 36) `using` parPair
main = print pair 
  where pair = (fib 35, fib 36) `using` parPair

