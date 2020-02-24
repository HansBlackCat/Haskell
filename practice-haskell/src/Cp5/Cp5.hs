module Cp5.Cp5 where
{- Laziness and Infinite Structures -}

import Cp2.Cp2

import Data.List
import Data.List.Ordered as O

data TimeMachine = TM { manufacturer :: String
                      , year :: Integer }
                      deriving (Eq, Show)

-- Infin declar
timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

-- Infin
timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

-- 
timelyTakeThree = take 3 timelyIncMachines
timelyFindSpecial = find (\(TM {year=y}) -> y>2018) timelyIncMachines
timelyListZip = (\list -> zip [1..length list] list) "abcd"
--

-- Infi
allNumbers :: [Integer]
allNumbers = allNumbersFrom 1
allNumbersFrom :: Integer -> [Integer]
allNumbersFrom n = n : allNumbersFrom (n+1)

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- cycle
specialOffer :: [TimeMachine]
specialOffer = cycle [TM m 2005, TM m 1994, TM m 908]
  where m = "Timely Inc."

takeFourspecialOffer = take 4 specialOffer 
-- [TM {manufacturer = "Timely Inc.", year = 2005},TM {manufacturer = "Timely Inc.", year = 1994},TM {manufacturer = "Timely Inc.", year = 908},TM {manufacturer = "Timely Inc.", year = 2005}]

-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = [x, f x, f. f x, f. f. fx, ..]
fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n, n1) -> (n1, n+n1)) (0, 1)
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,..]

eratosSieve :: [Integer]
eratosSieve = 2 : 3 : minus [5,7..] (foldr O.union [] [[p*p, p*p+2*p..] | p <- tail eratosSieve])

