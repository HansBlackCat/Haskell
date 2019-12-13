import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as L --Lazy
import qualified Data.ByteString as S --Strict

main = do
    (fileName1:fileName2:_) <- getArgs
    copy fileName1 fileName2

copy source dest = do
    contents <- L.readFile source
    bracketOnError 
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            L.hPutStr tempHandle contents
            hClose tempHandle 
            renameFile tempName dest)
