{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Matrix.Static

x = 3::Int
safeGet
y = fromList [1..9] :: (Matrix 3 3 Int)

z = matrix (\(i,j) -> 2*i - j) :: Matrix 3 4 Int

zero_22 = zero :: Matrix 2 2 Int 
id_3 = identity @3

a = fromListUnsafe @4 @4 [1..16]
b = fromListUnsafe @4 @2 [4..12]
