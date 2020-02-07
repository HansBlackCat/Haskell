import Control.Parallel
import Control.Parallel.Strategies (rpar, rseq, Strategy, using, rparWith, rdeepseq, r0)
import Control.Exception
import Text.Printf
import System.Environment

-- ./strat3 +RTS -N2 -l
-- `using` parPair rseq rseq Tot time: 0.982s
-- `using` parPair rpar rpar Tot time: 1.030s
-- `using` parPair rdeepseq rdeepseq Tot time: 1.013s

-- evaluated with fully evalutated NF -> parameterized strategy
main = print pair 
  where pair = (fib 35, fib 36) `using` parPair rdeepseq rdeepseq

-- id parameterized strategy
-- r0 performs *no* evaluation.
-- evalPair (evalPair rpar r0) (evalPair rpar r0) :: Strategy ((a, b), (c, d))

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do 
  a' <- sa a 
  b' <- sb b 
  return (a', b')

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parPair :: Strategy a -> Strategy b -> Strategy (a, b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
