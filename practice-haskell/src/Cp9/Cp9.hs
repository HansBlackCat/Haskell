{-# LANGUAGE ScopedTypeVariables #-}

module Cp9.Cp9 where


import System.Environment
import System.IO
import Data.Char
import Data.String
import Data.List
import System.IO
import Control.Monad
import System.Random
import Control.Monad.Loops
import System.FilePath

data Person = Person { firstName :: String
                     , lastName :: String }
            deriving (Show,Eq,Ord)
data Client i = Govorg { clientId :: i
                       , clientName :: String }
              | Company { clientId :: i
                        , clientName :: String
                        , person :: Person
                        , duty :: String }
              | Individual { clientId :: i
                           , person :: Person }
              deriving (Show,Eq,Ord)

testClient :: [Client String]
testClient = [Govorg "Gov" "Kor"
             ,Company "Samsung" "Lee" (Person "Lee" "Kim") "CEO"
             ,Individual "Me" (Person "Lee" "Mann")]

mainLineBuffer :: IO()
mainLineBuffer = hSetBuffering stdout LineBuffering -- enable line buffering
-- make system flusth the contents after each newline

main1 :: IO()
main1 = putStrLn "Hello Begining Haskell!"

main2 :: IO()
main2 = do putStrLn "Where do you want to travel?"
           place <- getLine
           let year = length place * 10
           putStrLn $ "You should travel to year " ++ show year

main3 :: IO()
main3 = do putStrLn "First name?"
           fName <- getLine
           putStrLn "Last name?"
           lName <- getLine
           putChar '>' >> putChar ' '
           print $ Person fName lName

-- haskeline library
-- support good APIs for cli applications

main4 :: IO()
main4 = do s <- getLine
           let upperS = fmap toUpper s
           putStrLn upperS >> putStrLn upperS

main4' :: IO()
main4' = do upperS <- fmap (fmap toUpper) getLine
            putStrLn upperS >> putStrLn upperS

createVIPList :: Show a => [Client a] -> IO [Client a]
createVIPList =
  foldM (\lst c -> do putStrLn $ "\nShould " ++ show c
                                             ++ "be included as VIP? "
                      answer <- getLine
                      case answer of
                        'Y':_ -> return $ c:lst
                        'y':_ -> return $ c:lst
                        _     -> return lst) []


-- randomRIO :: Random a => (a,a) -> IO a
-- unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
main5 :: IO()
main5 = do (initial :: Int) <- fmap read getLine
           jumps <- unfoldrM (\_ -> do next <- randomRIO (0,10)
                                       if next == initial
                                         then return Nothing
                                         else return $ Just (next,next)
                             ) initial
           print $ take 10 jumps


-- purified random lists
getJumps :: StdGen -> Int -> [Int]
getJumps gen initial = unfoldr (\g -> let (next, nextG) = randomR (0,3000) g
                                      in if next == initial
                                           then Nothing
                                           else Just (next,nextG)
                               ) gen

main5' :: IO()
main5' = do (initial :: Int) <- fmap read getLine
            gen <- getStdGen
            print $ take 10 $ getJumps gen initial


main6 :: IO()
main6 = do clients <- lines <$> readFile "clients.db"
           clientsAndWinners <- mapM (\c -> do (winner :: Bool) <- randomIO
                                               (year :: Int) <- randomRIO (0,3000)
                                               return (c, winner, year)
                                     ) clients
           writeFile "clientWinners.db" $ concatMap show clientsAndWinners

-- in more effective way(with file handler)
main6' :: IO()
main6' = do (inFile:outFile:_) <- getArgs
            inHandle <- openFile inFile ReadMode
            outHandle <- openFile outFile WriteMode
            loop inHandle outHandle
            hClose inHandle
            hClose outHandle
         where loop inHandle outHandle =
                 do isEof <- hIsEOF inHandle
                    if not isEof
                      then do client <- hGetLine inHandle
                              (winner :: Bool) <- randomIO
                              (year :: Int) <- randomRIO (0,3000)
                              hPutStrLn outHandle $ show (client,winner,year)
                              loop inHandle outHandle
                      else return ()

main6'' :: IO()
main6'' = do (inFile:outFile:_) <- getArgs
             withFile inFile ReadMode $ \inHandle ->
               withFile outFile WriteMode $ \outHandle -> do
                 -- Write with UTF8 format
                 hSetEncoding outHandle utf8
                 loop inHandle outHandle
          -- Same as main6'
          where loop inHandle outHandle = undefined

-- Due to difference between file path syntax between mac,linux and window(/ or \), we recommend to use library named `filepath`
-- `</>`

main7 = do (file:_) <- getArgs
           let (folder, _) = splitFileName file
           withFile (folder </> "example") WriteMode $ \handle -> undefined -- etc..

