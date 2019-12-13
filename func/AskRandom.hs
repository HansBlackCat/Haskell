import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber ::StdGen -> IO ()
askForNumber gen = do
    let (randNum, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "<< 1 - 10 Choose >>"
    numberChoose <- getLine
    when (not $ null numberChoose) $ do
        let numberInt = read numberChoose 
        if randNum == numberInt
            then putStrLn "You're Correct!"
            else putStrLn $ "Nope! Answer was " ++ show randNum
        askForNumber newGen
