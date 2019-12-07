import Data.List.Ordered

primeNum = sieveEra [2..]
    where sieveEra [] = []
          sieveEra (x:xs) = x : sieveEra (minus xs (tail [x*1, x*2..]))
