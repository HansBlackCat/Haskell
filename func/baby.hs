doubleMe x = x+x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =if x > 100 then x else x*2

doubleSmallNumber' x = (if x>100 then x else x*2) + 1

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2*pi*r

circumference' :: Double -> Double
circumference' r = 2*pi*r

lucky :: Int -> String
lucky 7 = "LUCKY SEVEN"
lucky x = "Be-Beep!"

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

trd :: (a, b, c) -> c
trd (_, _, z) = z

head' :: [a] -> a
head' [] = error "Empty list"
head' (x: _) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "Element: " ++ show x
tell (x:y:[]) = "Elements: " ++ show x ++ ", "  ++ show y
