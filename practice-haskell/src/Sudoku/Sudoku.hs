module Sudoku.Sudoku where

import Data.Char
import Data.List

data AvailableGrid = AvailableList [Int]
                   | IsJust Int
                   deriving (Show, Eq, Ord)
type PackedGrid = [(AvailableGrid, (Xval, Yval))]

type Xval = Int
type Yval = Int
type Aval = Int
type GeneralTupleStyle = [(Aval,(Xval,Yval))]


interpret :: String -> GeneralTupleStyle
interpret input
  | length input == 81 =
    zip (fmap digitToInt input) [(x,y)|y<-[0..8],x<-[0..8]]
  | otherwise          = error "Length of String is not 81"

draw :: GeneralTupleStyle -> IO ()
draw []     = putStrLn ""
draw (x:xs) = do putStr . show $ fst x
                 if fst (snd x) == 8
                   then if or $ fmap (snd (snd x) ==) [2,5]
                          then do putStrLn ""
                                  putStr "- - - + - - - + - - -\n"
                                  draw xs
                          else do putStrLn ""
                                  draw xs
                   else if or $ fmap (fst (snd x) ==) [2,5]
                          then do putStr " | "
                                  draw xs
                          else do putStr " "
                                  draw xs

tupToGrid :: GeneralTupleStyle -> PackedGrid
tupToGrid = undefined

-- TEST function
testInput :: String
testInput = "060080204000002000802100007750004080000000003310009060405600001000003000070040609"
testDraw :: IO ()
testDraw = draw $ interpret testInput
