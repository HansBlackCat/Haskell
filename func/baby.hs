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

firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ "is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "Underweight!"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat!"
    | otherwise   = "Who are you?"
    where bmi = weight / height^2
          skinny = 18.5
          normal = 25.0
          fat = 3.0
    
max' :: (Ord a) => a -> a -> a
max' a b
    | a<=b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a-> Ordering
a `myCompare` b
    | a==b = EQ
    | a<=b = LT
    | otherwise = GT
    
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBMIs :: [(Double, Double)] -> [Double]
calcBMIs xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height^2
    
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2*pi*r*h
        topArea = pi*r^2
    in sideArea + 2*topArea

calcBMIs' :: [(Double, Double)] -> [Double]
calcBMIs' xs = [bmi | (w,h) <- xs, let bmi = w/h^2]
    
head'' :: [a] -> a
head'' xs = case xs of [] -> error "Nonono"
                       (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "Empty"
                                               [x] -> "Singleton list"
                                               xs -> "Longer list"


                      
