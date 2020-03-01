module Cp7.C7 where

import Data

import Control.Monad 
import Control.Lens
import Data.List
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)
import Data.Set (Set)
import qualified Data.Set as S

-- monad transormers

tab1 = [1,2,3] >>= \x -> [2*x, 3*x]
-- [2,3,4,6,6,9]
tab1c = concat $ map (\x -> [2*x, 3*x]) [1,2,3]
-- [2,3,4,6,6,9]

tab2 = do 
  x <- [1,2,3]
  y <- [7,8,9]
  return $ x*y
-- [7,8,9,14,16,18,21,24,27]

-- join :: Monad m => m (m a) -> m a
-- x >>= f == join $ fmap f x

tab3 = mapM (\x -> Just (2*x)) [1,2,3]
-- Just [2,4,6]
tab3a = mapM_ (\x -> Just (2*x)) [1,2,3]
-- Just ()

tab4 :: Integer -> [Integer]
tab4 n = [n-1, n+1]
tab4a n = [16, n^2]
tab4p = tab4 3 `mplus` tab4a 10
-- [2,4,16,100]

tab5 = do 
  x <- [1..10]
  guard (x>6)
  return x
-- [7,8,9,10]

tab6 = msum [[1], [2,3], [4,6,8]]
-- [1,2,3,4,6,8]
tab6a = msum [Just 1, Nothing, Just 3, Nothing]
-- Just 1

{- 7-2
find_ :: (a -> Bool) -> [a] -> Maybe a 
find_ f l = 
  let listJusted = map (\x -> Just x) l
-}