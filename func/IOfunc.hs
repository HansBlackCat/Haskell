import Control.Monad
import Data.Char

main = {- forever $  -}  do -- <forever> recursion endless
    -- <putStr, putChar>
    putStr "Hey "
    putStr "You!"
    -- > Hey You!
    
    -- <print>
    print True
    print 2
    print "Red"
    print [3.4, 2]
    -- > True 2 Red [3.4,2] // print 
    
    -- <when>
    input <- getLine
    when (input == "Sword") $ do
        putStrLn input
    -- Same> if (ipt == "Sword") then putStrLn ipt else return ()    
    
    -- <sequence>
    rs <- sequence [getLine, getLine, getLine]
    print rs
    -- Same> a<-gL b<-gL c<-gL print [a,b,c]

    -- <mapM, mapM_>
    -- sequence + map => mapM
    mapM print [1,2,3]
    -- Same> sequence $ map print [1,2,3]
    mapM_ print [1,2,3]
    -- Similar, but don't return ()

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs


