

solveRPN :: String -> Double
solveRPN exp = head (foldl folder [] stack)


