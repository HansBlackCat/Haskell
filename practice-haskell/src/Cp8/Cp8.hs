module Cp8.Cp8 where

import Data.List (nub, sort)
import Control.DeepSeq
import Control.Monad.Par

-- Software Transactional Memory (STM)
-- AMQP?

nFactors :: Integer -> [Integer]
nFactors n = sort . nub $ findFactors n

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                    in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m         = n
  | n `mod` m == 0 = m
  | otherwise      = findFactor n (m+1)

-- monad-par
-- spawnP :: NFData a => a -> Par (IVar a)
-- Ivar is `future` in other lang
findTwoFactors :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)
