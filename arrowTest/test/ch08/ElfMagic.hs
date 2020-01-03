module ElfMagic where

import qualified Data.ByteString.Lazy as L 

hasElfMagic :: L.ByteString -> Bool 
hasElfMagic content = L.take 4 content == L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> Bool
isElfFile path = do 
  content <- L.readFile path
  return $ hasElfMagic content

