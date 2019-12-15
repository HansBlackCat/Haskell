

folder (x:y:ys) "*" = (y * x):ys
folder (x:y:ys) "+" = (y + x):ys
folder (x:y:ys) "-" = (y - x):ys
folder (x:y:ys) "/" = (y / x):ys
folder (x:y:ys) "^" = (y** x):ys
folder (x:ys) "ln"  = log x:ys
folder xs "sum"     = [sum xs]
folder ys y         = read y:ys


solveRPN :: String -> Double
solveRPN = head. foldl folder []. words




