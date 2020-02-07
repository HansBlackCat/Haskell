-- Total time: 1.514s

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

main = print pair 
  where pair = (fib 35, fib 36)