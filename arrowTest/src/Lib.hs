module Lib(module Lib, 
           module ArrowFun,
           ) where

import ArrowFun

someFunc :: IO ()
someFunc = putStrLn "someFunc"
