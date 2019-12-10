import Control.Monad

main = do
    -- <forM>
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Color pick: " ++ show a
        color <- getLine
        return color)
    putStrLn "Colors 1,2,3, and 4: "
    mapM putStrLn colors
    print colors
    -- mapM <IOfunction> <List> == forM <Obj> <List>
    


