module Sudoku.Sudoku2 where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.List
import Data.Bits
import Data.Word

-- ---------------------------------------------------------------------------
-- Base data
-- ---------------------------------------------------------------------------
type Base = Word32
type Space = U.Vector Base

totCellNums :: Int
totCellNums = 81

-- ---------------------------------------------------------------------------
-- Tools
-- ---------------------------------------------------------------------------
-- is 2^28?
newSingle :: Base -> Bool
newSingle n = shiftR n 27 == 2

setSolution :: Base -> Base
setSolution n = setBit n 27

maskChoices :: Base -> Base
maskChoices n = n .&. 0x07FFFFFF

countBits :: Base -> Base
countBits 0 = 0
countBits n = (cBLH 16 0xFFFF . cBLH 8 0xFF00FF . cBLH 4 0x0F0F0F0F . cBLH 2 0x33333333 . cBLH 1 0x55555555) n
  where cBLH s mask n = (n .&. mask) + (n `shiftR` s) .&. mask

minus :: Base -> Base -> Base
minus x y
  | maskChoices (x .&. y) == 0 = x
  | otherwise = z .|. (shiftL (countBits z) 28)
    where z = maskChoices $ x .&. (complement y)

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------
rows :: Space -> Space
rows = id

columns :: Space -> Space
columns vec = U.map (U.unsafeIndex vec) loc
  where loc = U.fromList [r*9+c | c<-[0..8], r<-[0..8]]

subGrids :: Space -> Space
subGrids vec = U.map (U.unsafeIndex vec) loc
  where loc = U.fromList [i+bc+br | br<-[0,27,54], bc<-[0,3,6], i<-[0,1,2,9,10,11,18,19,20]]

boolSpace :: V.Vector (U.Vector Bool)
boolSpace = V.generate totCellNums discriminator
  where
    discriminator idx1 = U.zipWith3 (\r c s -> r||c||s) sameR sameC sameG
      where
        sameR = U.generate totCellNums (\idx2 -> idx1 `div` 9 == idx2 `div` 9)
        sameC = U.generate totCellNums (\idx2 -> idx1 `mod` 9 == idx2 `mod` 9)
        sameG = U.generate totCellNums (\idx2 -> subGrid idx1 == subGrid idx2)
          where
            subGrid idx = (idx `div` 27, (idx `div` 3) `mod` 3)

--
zeroOptimize :: Space -> Maybe Space
zeroOptimize space = U.foldM baseOptimize space $ U.findIndices newSingle space

baseOptimize :: Space -> Int -> Maybe Space
baseOptimize space index =
  let sIndex = space U.! index
      bIndex = boolSpace V.! index
      newSpace = U.zipWith opt bIndex space
        where opt a b
                | a&&(b==sIndex) = setSolution sIndex
                | a = minus b sIndex
                | otherwise = b
  in undefined

-- Debug
testRBase = 32 :: Base
testRSpace = U.fromList [0..81] :: Space
