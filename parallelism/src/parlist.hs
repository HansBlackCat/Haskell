module ParList where

import Control.Parallel.Strategies hiding (parList, evalList)

-- evalList structure
evalList' :: Strategy a -> Strategy [a]
evalList' strat [] = return []
evalList' strat (x:xs) = do 
  a' <- strat x 
  as' <- evalList' strat xs 
  return (a':as')

-- parList structure
parList' :: Strategy a -> Strategy [a]
parList' strat = evalList' (rparWith strat)

