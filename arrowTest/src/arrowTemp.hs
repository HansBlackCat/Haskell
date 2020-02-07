{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import Data.Char
-- import System.Random

--------------------------------------------------------------
-- main
main = do
    putStrLn $ "f = arr even $ 42 ::: " ++ (show f)
    putStrLn $ "g = arr ord >>> arr even $ '*' ::: " ++ (show g) 
    putStrLn $ "h = (first $ arr even) (42, 42) ::: " ++ (show h)
    putStrLn $ "fA ::: " ++ (show $ fA 10)
    putStrLn $ "idA 3 ::: " ++ (show $ idA 3)
    putStrLn $ "plusOne 34 ::: " ++ (show $ plusOne 34)
    putStrLn $ "plusOneAndTwo 3 ::: " ++ (show $ plusOneAndTwo 3)
    putStrLn $ "addAb (*3) (+20) 5 ::: " ++ (show $ addAb (*3) (+20) 5)
    putStrLn $ "addAbs (*3) (+20) 5 ::: " ++ (show $ addAbs (*3) (+20) 5)
    putStrLn $ "meanTest [1,2,3,10,9] ::: " ++ (show $ meanTest [1,2,3,10,9])
-- /main
--------------------------------------------------------------

-- show
f = arr even $ 42
g = arr ord >>> arr even $ '*'
h = (first $ arr even) (42, 42)
-- /show

--------------------------------------------------------------
-- proc: Make arrow(Arrow Abstraction)
-- -<: Arrow Apply
-- proc Notation, and -< notation
idA :: a -> a 
idA = proc a -> returnA -< a 

plusOne :: Int -> Int 
plusOne = proc a -> returnA -< (a+1)
plusTwo :: Int -> Int 
plusTwo = proc a -> plusOne -< (a+1)
plusTwoBis :: Int -> Int
plusTwoBis = proc a -> do
  b <- plusOne -< a 
  plusOne -< b 
plusFive :: Int -> Int 
plusFive = proc a -> do 
  b <- plusOne -< a 
  c <- plusOne -< b 
  d <- plusTwo -< c 
  plusOne -< d
plusOneAndTwo :: Int -> (Int, Int)
plusOneAndTwo = proc a -> do 
  (b1, b2) <- (\x -> (x, x)) -< a
  (\(x, y) -> (plusOne x, plusTwo y)) -< (b1, b2)
-- /proc Notation, and -< notation
--------------------------------------------------------------

addAa :: Arrow a => a b Int -> a b Int -> a b Int 
addAa f g = proc x -> do 
  y <- f -< x
  z <- g -< x
  returnA -< y + z 

addAb f g = arr (\ x -> (x, x)) >>>
  first f >>> arr (\ (y, x) -> (x, y)) >>>
  first g >>> arr (\ (z, y) -> y + z)
addAbs f g = 
  f &&& g >>>
  arr (\(y, z) -> y + z)

meanTest =
  sum &&& length >>>
  arr (\(x, y) -> x `div` y)



fA :: Int -> (Int, Int)
fA = proc x -> do
  y  <- (2 *) -< x
  z1 <- (+ 3) -< y
  z2 <- (subtract 5) -< y
  returnA -< (z1, z2)

-- addAb :: Arrow a => a b Int -> a b Int 
{-
addAb f g = arr (\x -> (x, x)) >>>
  first f >>> arr (\(y, x) -> (x, y)) >>>
  first g >>> arr (\(z, y) -> y + z)

meanA a = arr (\x -> (sum x, length x)) >>> 
  arr (\(tempA, tempB) -> tempA/tempB)

-}

