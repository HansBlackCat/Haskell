import Control.Parallel
import Control.Parallel.Strategies (rpar, Strategy, using)
import Text.Printf
import System.Environment

-- ./strat2 +RTS -N2 -l
-- Total time: 0.960s

main = print pair 
  where pair = (fib 35, fib 36) `using` parPair

-- same as Control.Parallel.Strategies (evalTuple2)
-- type Strategy a = a -> Eval a
evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair sa sb (a,b) = do 
  a' <- sa a 
  b' <- sb b 
  return (a', b')

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parPair :: Strategy (a, b)
parPair = evalPair rpar rpar 
