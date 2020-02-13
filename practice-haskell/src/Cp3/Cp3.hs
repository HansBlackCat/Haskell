{-# LANGUAGE LambdaCase #-}

module Cp3.Cp3 where

import Cp2.Cp2

swap (x,y,z) = (y,z,x)

duplicate x = (x,x)

nothing _ = Nothing

index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs 
  in (n+1,x):indexed

maybeA [] = 'a'

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of 
  "Hans" -> "It's you"
  _ -> "Hello " ++ name
  ) names

sayHelloWithPragma :: [String] -> [String]
sayHelloWithPragma names = map (\case "Hans" -> "It's you"
                                      name -> "Hello " ++ name
                               ) names

multiplyByN :: Integer -> Integer -> Integer
multiplyByN n = \x -> n*x 

filterOnes = filter filterOnest
  where 
    filterOnest n
      | n == 1    = True 
      | otherwise = False

filterANumber a = filter aNumber 
  where 
    aNumber n 
      | n == a    = True 
      | otherwise = False

filterNot = filter rev 
  where 
    rev = 
      (\case True -> False
             False -> True)

filterGovOrgs = filter isGovOrgs
  where 
    isGovOrgs = 
      (\case GovOrg a -> True
             _ -> False)
