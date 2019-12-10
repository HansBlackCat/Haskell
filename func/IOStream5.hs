import System.IO

main = do
    withFile "haiku2.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)

-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
