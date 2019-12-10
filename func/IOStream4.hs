import System.IO

main = do
    handle <- openFile "haiku.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
