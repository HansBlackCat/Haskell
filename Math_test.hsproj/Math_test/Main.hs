{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Matrix as Mat 
import Data.Vector as Vec

import Graphics.Rendering.Chart.Easy
import 


mat_a = matrix 4 4 $ \(i, j) -> 2*i+j

id_4 = Mat.identity 4 

fromListMatrix = Mat.fromList 3 3 [0..]

matToLists = Mat.toLists mat_a 

getElemofMata = getElem 2 3 mat_a

getRowofMata = getRow 3 mat_a 

transMat_a = transpose mat_a

extendMata = extendTo 9 5 5 mat_a

mapRowMata = mapRow (\_ x -> x+1) 4 mat_a

subMatricMata = submatrix 1 2 2 3 mat_a 

luDtempMat = Mat.fromList 3 3 [1,2,0,0,2,1,2,0,2]
-- U, L, P, Q, d, e
-- PMQ = LU
luDMata = luDecomp' luDtempMat
-- Cholsky composition
-- M = LL^T
cholMata = cholDecomp $ Mat.fromList 3 3 [2,-1,0,-1,2,-1,0,-1,2]

detTest = detLU luDtempMat


signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))
    


