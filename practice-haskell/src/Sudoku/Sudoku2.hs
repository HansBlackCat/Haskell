module Sudoku.Sudoku2 where

import Data.List
import Control.Monad

cross a b = [(x,y)|x<-a,y<-b]

solve f = foldr (=<<) [f] $
  do
    a <- lSmallGrid
    b <- lSmallGrid
    guard (f (a, b) == 0)
    let ps = cross [a] lSmallGrid
          <> cross lSmallGrid [b]
          <> cross (cross [fst a] [0..2]) (cross [fst b] [0..2])
    pure $ \g -> do
        c <- [1..9] \\ map g ps
        pure $ \p -> if p == (a, b) then c else g p
  where lSmallGrid = cross [0..2] [0..2]

get s ((a,b), (c,d)) = map (read . (:[])) s !! (3*a + b + 9*(3*c + d))

put f = putStrLn $ unlines [concat [' ': show (f (a,b)) | a<-cross [0..2] [0..2]] | b<-cross [0..2] [0..2]]

main = mapM_ put . solve $ get
 "060080204000002000802100007750004080000000003310009060405600001000003000070040609"

readI :: String -> Integer
readI = read
