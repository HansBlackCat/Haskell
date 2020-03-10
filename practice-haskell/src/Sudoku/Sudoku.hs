{-# LANGUAGE LambdaCase #-}
module Sudoku.Sudoku where

import Data.Char
import Data.List

data AvailableGrid = AvailableList [Int]
                   | IsJust Int
                   deriving (Show, Eq, Ord)

type PackedGrid = [(AvailableGrid, (Xval, Yval))]
type Board = [PackedGrid]

type Xval = Int
type Yval = Int
type Aval = Int
type GeneralTupleStyle = [(Aval,(Xval,Yval))]

-- ---------------------------------------------------------------------------
-- Base Indexing Functions
-- ---------------------------------------------------------------------------
-- [(0,0), (1.0), .. (8,8)]
baseIndex :: [(Int, Int)]
baseIndex = [(x,y) | y<-[0..8], x<-[0..8]]

-- Small Boxs
{-
1 | 2 | 3
- + - + -
4 | 5 | 6
- + - + -
7 | 8 | 9
-}
-- 012 345 678
baseBox :: (Int,Int) -> Int
baseBox (a, b)
  | and [a<3,b<3] = 1
  | and [a<6,b<3] = 2
  | and [a<9,b<3] = 3
  | and [a<3,b<6] = 4
  | and [a<6,b<6] = 5
  | and [a<9,b<6] = 6
  | and [a<3,b<9] = 7
  | and [a<6,b<9] = 8
  | and [a<9,b<9] = 9
  | otherwise     = error "Indexing out-of range"

baseUnwrapper :: AvailableGrid -> Int
baseUnwrapper typ = case typ of
                      IsJust a        -> a
                      AvailableList _ -> 0

-- ---------------------------------------------------------------------------
-- Boxing Functions
-- ---------------------------------------------------------------------------
interpret :: String -> GeneralTupleStyle
interpret input
  | length input == 81 =
    zip (fmap digitToInt input) baseIndex
  | otherwise          = error "Length of String is not 81"

tupToUntunedGrid :: GeneralTupleStyle -> PackedGrid
tupToUntunedGrid [] = []
tupToUntunedGrid (x:xs) =
  if fst x == 0
    then (AvailableList [1..9], snd x) : tupToUntunedGrid xs
    else (IsJust $ fst x, snd x) : tupToUntunedGrid xs

untunedGridToUntunedBoard :: PackedGrid -> Board
untunedGridToUntunedBoard [] = []
untunedGridToUntunedBoard list = a : untunedGridToUntunedBoard b
  where (a,b) = splitAt 9 list

-- Unboxing
unboxingGridToTuple :: PackedGrid -> GeneralTupleStyle
unboxingGridToTuple [] = []
unboxingGridToTuple (x:xs) =
  case fst x of
    IsJust a        -> (a, snd x) : unboxingGridToTuple xs
    AvailableList _ -> (0, snd x) : unboxingGridToTuple xs

toDrawable :: Board -> GeneralTupleStyle
toDrawable = unboxingGridToTuple . concat

-- ---------------------------------------------------------------------------
-- Tuning Functions
-- ---------------------------------------------------------------------------

--- HERER

baseTuning :: PackedGrid -> PackedGrid
baseTuning g@(x:xs) =
  let tunedList = undefined
  in undefined
  where justList = dropWhile (<1).sort.fmap (baseUnwrapper.fst)

-- input : head testBoard
-- dropWhile (<1) . sort $ fmap (choose.fst) h
-- AvailableList :: [Int] -> AvailableGrid

tuningBoard :: Board -> Board
tuningBoard = tuningBox . tuningColumn $ tuningRow
  where tuningRow = undefined
        tuningColumn = tuningRow . transpose
        tuningBox = undefined


-- ---------------------------------------------------------------------------
-- Solving Functions
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- IO Functions
-- ---------------------------------------------------------------------------
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
testBoard :: Board
testBoard = untunedGridToUntunedBoard . tupToUntunedGrid $ interpret testInput
testDraw :: IO ()
testDraw = draw $ interpret testInput
