import System.Random

booleanTrans :: Bool -> String
booleanTrans a  
    | a == True = "Front"
    | otherwise = "Rear"

threeCoins :: Int -> (String, String, String)
threeCoins a =
    let gen = mkStdGen a
        (firstCoin, newGen) = random gen :: (Bool, StdGen)
        (secondCoin, newGen') = random newGen :: (Bool, StdGen)
        (thirdCoin, newGen'') = random newGen' :: (Bool, StdGen)
    in (booleanTrans firstCoin, booleanTrans secondCoin, booleanTrans thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (a,b) = random gen
    in a : randoms' b

finiteRandoms' :: (RandomGen g, Random a, Num n, Eq n) => g -> n -> [a]
finiteRandoms' _ 0 = []
finiteRandoms' gen num = 
    let (val, val_next) = random gen
    in val : finiteRandoms' val_next (num-1) 
