import Data.List

main = do
    contents <- getContents
    let threes = groupOf 3 (map read $ lines contents)
        roadsys = map (\[a,b,c] -> Selection a b c) threes
        path = optimalPath roadsys
        pathString = concat $ map (show. fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The Best path you can take: "
    putStrLn pathString
    putStrLn $ "Time taken: "
    putStrLn $ show pathTime

data Selection = Selection {getA :: Int,
                            getB :: Int,
                            getC :: Int} deriving (Show)

type RoadSystem = [Selection]

toLondon :: RoadSystem
toLondon = [ Selection 50 10 30
           , Selection 5 90 20
           , Selection 40 2 25
           , Selection 10 8 0 ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]           

groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs: groupOf n (drop n xs)

roadStep :: (Path, Path) -> Selection -> (Path, Path)
roadStep (pathA, pathB) (Selection a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        forwardTimeToB = timeB + b
        crossTimeToA = timeB + b + c
        crossTimeToB = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A, a):pathA
                        else (C, c):(B,b):pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B, b):pathB
                        else (C, c):(A,a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadsys =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadsys
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
           then reverse bestAPath
           else reverse bestBPath

