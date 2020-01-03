module GlobRegex where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^': globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = '.':globToRegex' cs
globToRegex' ('[':'!':c:cs') = "[^" ++ c:charClass cs'
globToRegex' ('[':c:cs) = '[' :c:charClass cs 
globToRegex' ('[':_) = error "unterminated char class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c 
  | c `elem` regexChars = '\\':[c]
  | otherwise = [c]
    where regexChars = "\\+()^$.{}"
    
charClass :: String -> String 
charClass (']':cs) = ']':globToRegex' cs
charClass (c:cs) = c: charClass cs 
charClass [] = error "undeterminated chat class" 
