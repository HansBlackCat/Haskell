module SumFile where

main = do
  contents <- getContents
  print $ sum . map read . words $ contents
  