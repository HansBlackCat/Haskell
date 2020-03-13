{-# LANGUAGE LambdaCase #-}
module Sudoku.Sudoku where

import Data.Char
import Data.List
import System.IO

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

baseMonadic :: AvailableGrid -> [Int]
baseMonadic (AvailableList a) = a
baseMonadic (IsJust a) = [a]

baseLoc :: (Int,Int) -> Int
baseLoc (a,b) = a + 9*b

substitute :: Int -> [Int] -> PackedGrid -> PackedGrid
substitute i list grid =
  let (a,b) = splitAt i grid
  in a ++ ((subs list, snd $ head b) : tail b)
  where subs list = case length list of
                      1 -> IsJust (head list)
                      _ -> AvailableList list

-- ---------------------------------------------------------------------------
-- Boxing Functions
-- ---------------------------------------------------------------------------
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

tupToGrid :: GeneralTupleStyle -> Board
tupToGrid = untunedGridToUntunedBoard . tupToUntunedGrid

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

justList :: PackedGrid -> [Int]
justList = dropWhile (<1).sort.fmap (baseUnwrapper.fst)

baseTuning :: [Int] -> PackedGrid -> PackedGrid
baseTuning _ [] = []
baseTuning jList (x:xs) =
  case fst x of
    AvailableList i
      -> if length (i \\ jList) == 1
            then (IsJust (head $ i \\ jList), snd x) : baseTuning jList xs
            else (AvailableList (i \\ jList), snd x) : baseTuning jList xs
    IsJust _        -> x : baseTuning jList xs

-- input : testBoard
-- dropWhile (<1) . sort . fmap (choose.fst) . head
-- AvailableList :: [Int] -> AvailableGrid

tuningBoard :: Board -> Board
tuningBoard = sortingBoard . tuningBox . tuningColumn . tuningRow
  where tuningRow [] = []
        tuningRow (y:ys) = baseTuning (justList y) y : tuningRow ys
        tuningColumn = tuningRow . transpose
        tuningBox = tuningRow . untunedGridToUntunedBoard . sortBy (\a b -> compare (baseBox$snd a) (baseBox$snd b)) . concat

-- baseBox :: (Int,Int)->Int
-- sortBy (\a b -> compare (baseBox$snd a) (baseBox$snd b))

-- Only for perfect matching, or infiloop
perfectTuning :: Board -> Board
perfectTuning board =
  let tuned = tuningBoard board
  in if not $ errorPicker tuned
        then perfectTuning tuned
        else tuned
--let iter@(x:xs) = (fmap errorPicker $ iterate errorPicker board)

sortingBoard :: Board -> Board
sortingBoard = transpose . untunedGridToUntunedBoard . sortOn snd . concat

-- ---------------------------------------------------------------------------
-- Solving Functions
-- ---------------------------------------------------------------------------

errorPicker :: Board -> Bool
errorPicker bo = and $ fmap and [checkBox bo, checkColumn bo, checkRow bo]
  where checkRow [] = []
        checkRow (pac:ys) = fitting pac:checkRow ys
        checkColumn = checkRow . transpose
        checkBox = checkRow . untunedGridToUntunedBoard . sortBy (\a b -> compare (baseBox$snd a) (baseBox$snd b)) . concat

fitJust :: PackedGrid -> Bool
fitJust pac = justList pac == nub (justList pac)
fitAvai :: [Int] -> PackedGrid -> Bool
fitAvai list pac = and $ boollist pac
  where boollist [] = []
        boollist (y:ys) =
          case fst y of
            AvailableList i -> ((i \\ list) == i) : boollist ys
            _               -> boollist ys
fitting :: PackedGrid -> Bool
fitting pac = fitJust pac && fitAvai (justList pac) pac

-- concat board
firstAvail :: PackedGrid -> (AvailableGrid,(Xval,Yval))
firstAvail [] = (IsJust (-1), (-1,-1))
firstAvail (x:xs) =
  case fst x of
    AvailableList i -> (AvailableList i, snd x)
    IsJust _        -> firstAvail xs
firstAvailLoc :: PackedGrid -> Int
firstAvailLoc = baseLoc . snd . firstAvail





{-
solve =
  do (a,b) <- firstAvail $ concat board
     loc <- baseLoc b
-}
-- subtitute loc list grid
res1 = perfectTuning $ untunedGridToUntunedBoard test1
  where test1 = substitute 0 [1] (concat $ perfectTuning testBoard)

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

interpret :: String -> GeneralTupleStyle
interpret input
  | length input == 81 =
    zip (fmap digitToInt input) baseIndex
  | otherwise          = error "Length of String is not 81"

eulerIO :: IO [String]
eulerIO = do str <- readFile "/Users/hansblackcat/Documents/Git/Haskell/practice-haskell/src/Sudoku/sudoku.txt"
             return (lines str)

-- ---------------------------------------------------------------------------
-- TEST function
-- ---------------------------------------------------------------------------
testInput :: String
testInput = "060080204000002000802100007750004080000000003310009060405600001000003000070040609"
testInput2 :: String
testInput2 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
testInput3 :: String
testInput3 = "005000006070009020000500107804150000000803000000092805907006000030400010200000600"
testBoard :: Board
testBoard = tupToGrid $ interpret testInput
testDraw :: IO ()
testDraw = draw $ interpret testInput
testGrid :: PackedGrid
testGrid = zip [AvailableList [1,2,3,4,5],IsJust 3,AvailableList [1,3,5],AvailableList [2,4],IsJust 4, AvailableList [5]] [(x,0)|x<-[0..5]]
