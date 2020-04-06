module Sudoku.Sudoku2 where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.List
import Data.Bits
import Data.Word
import Data.Char
import Control.Monad
import Data.Maybe
import System.Environment (getArgs)
-- import qualified Data.Bits.Utils as MB


-- ---------------------------------------------------------------------------
-- Base data
-- ---------------------------------------------------------------------------
type Base = Word32
type Space = U.Vector Base

totCellNums :: Int
totCellNums = 81



-- ---------------------------------------------------------------------------
-- Debugging Function
-- ---------------------------------------------------------------------------
interpret :: String -> [Base]
interpret = map (read . (:[]))



-- ---------------------------------------------------------------------------
-- Tools
-- ---------------------------------------------------------------------------
-- Word 32

-- is 2^28?
newSingle :: Base -> Bool
newSingle n = shiftR n 27 == 2

-- turn on 28th bit
setSolution :: Base -> Base
setSolution n = setBit n 27

-- Word32
-- 00000000 00000000 00000000 00000000
-- 00000111 11111111 11111111 11111111
maskChoices :: Base -> Base
maskChoices n = n .&. 0x07FFFFFF

-- count on-bits
countBits :: Base -> Base
countBits 0 = 0
countBits n = (cBLH 16 0xFFFF . cBLH 8 0xFF00FF . cBLH 4 0x0F0F0F0F . cBLH 2 0x33333333 . cBLH 1 0x55555555) n
  where cBLH s mask n = (n .&. mask) + (n `shiftR` s) .&. mask

-- minus binary
minus :: Base -> Base -> Base
minus x y
  | maskChoices (x .&. y) == 0 = x
  | otherwise = maskChoices $ z .|. (shiftL (countBits z) 28)
  where
    -- bit of x is on, y is off
    z = maskChoices $ x .&. complement y

-- n == 1?
empty :: Base -> Bool
empty n = maskChoices n == 1

intersect :: Base -> Base -> Base
intersect x y = z .|. (shiftL (countBits z) 28)
  where z = maskChoices $ x .&. y

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

-- ---------------------------------------------------------------------------
-- Solve
-- ---------------------------------------------------------------------------
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
  in if U.any empty newSpace
       then return newSpace
       else if U.any newSingle newSpace
              then zeroOptimize newSpace
              else return newSpace

{-
lS :: Space -> Maybe Space
lS unit =
  let known = maskChoices . accumTally $ U.filter single unit
  in if dups known
       then Nothing
       else case minus (filterSingles . accumTally $ U.filter (not . single) unit) known of
              0 -> return unit
              s -> return $ replaceWith unit s
                where replaceWith :: U.Vector Base -> Base -> U.Vector Base
                      replaceWith unit s =
                        U.map (\u -> if maskChoices (s.&.u) /= 0
                                       then Sudoku.Sudoku2.intersect s u
                                       else u) unit
-}

-- ---------------------------------------------------------------------------
-- Debug
-- ---------------------------------------------------------------------------
testRBase = 32 :: Base
testRSpace = U.fromList [0..81] :: Space
testSpace = U.fromList $ interpret "060080204000002000802100007750004080000000003310009060405600001000003000070040609"
