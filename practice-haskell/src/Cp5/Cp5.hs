{-# LANGUAGE BangPatterns #-}

module Cp5.Cp5 where
{- Laziness and Infinite Structures -}

import Cp2.Cp2

import Data.List
import Data.List.Ordered as O
import Control.DeepSeq

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
eratosSieve = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail eratosSieve])


-- Lazy evaluation
sumForce :: [Integer] -> Integer 
sumForce xs = sumForceBase xs 0
  where sumForceBase [] z = z 
        sumForceBase (y:ys) z = 
          let s = z + y in seq s sumForceBase ys s

-- Pattern Matching and Laziness
-- {-# LANGUAGE BangPatterns #-}
sumYears :: [TimeMachine] -> Integer 
sumYears xs = sumYearsBase xs 0
  where sumYearsBase [] z           = z
        -- Matching
        sumYearsBase (TM _ !y:ys) z = 
          let !s = z+y in sumYearsBase ys s

-- Irrefutable Pattern Matching
-- Delay the computation
irrefTest = Just 3
irreFunc = case irrefTest of ~(Just e) -> e + 3

-- IO Test
iOTest = do 
  a <- getLine
  return a

-- profiling
{- in stack.yaml
build:
  library-profiling: true
  executable-profiling: true
-}
-- stack build
-- stack exec practice-haskell-exe -- +RTS -p -h -RTS
-- hp2ps -c practice-haskell-exe.hp
iOResult = foldl' (*) 1 [1..100000]


-- Strict Field
data ListL a = ListL !Integer [a]

-- https://wiki.haskell.org/Performance/Data_types
-- unpacking tuple is OK, but not list
data ClientUnP = GovOrgUnP {-# UNPACK #-} !Int String 
               | CompanyUnP {-# UNPACK #-} !Int String Person String 
               | IndividualUnP {-# UNPACK #-} !Int Person 
               deriving (Show)
-- data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- import Control.DeepSeq
instance NFData Gender where 
  rnf Male = Male `deepseq` ()
  rnf Female = Female `deepseq` ()

instance NFData Client where
  rnf (GovOrg i) = i `deepseq` ()
  rnf (Company i n (Person f l g) r) = i `deepseq` n `deepseq` f `deepseq` l `deepseq` g `deepseq` r `deepseq` ()
  rnf (Individual (Person f l g) b) = f `deepseq` l `deepseq` g `deepseq` b `deepseq` ()

