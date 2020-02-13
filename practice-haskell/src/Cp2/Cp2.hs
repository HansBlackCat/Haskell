{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cp2.Cp2 where

data Client0 = GovOrg0     String
             | Company0    String Integer String String
             | Individual0 String String Bool
             deriving Show

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data ClientR = GovOrgR     { clientRName :: String }
             | CompanyR    { clientRName :: String
                           , companyId :: Integer
                           , person :: PersonR
                           , duty :: String}
             | IndividualR { person :: PersonR }
             deriving Show
data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } 
             deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female
            deriving Show

clientName1 :: Client -> String
clientName1 client = case client of
                       GovOrg  name                -> name
                       Company name id person resp -> name
                       Individual person ads       ->
                           case person of
                             Person fNm lNm gender -> fNm ++ " " ++ lNm

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0    = Nothing
ifibonacci 0            = Just 0
ifibonacci 1            = Just 1
ifibonacci n |otherwise = 
  let Just f1 = ifibonacci (n-1)
      Just f2 = ifibonacci (n-2)
  in Just (f1 + f2)

binom :: Integer -> Integer -> Integer
binom _ 0        = 1
binom x y | x==y = 1
binom n k        = (binom (n-1) (k-1)) + (binom (n-1) k)

-- Ackermann function
acker :: Integer -> Integer -> Integer
acker 0 n               = n+1
acker m 0 | m>0         = acker (m-1) 1
acker m n | m>0 && n>0  = acker (m-1) (acker m (n-1))

specialClient :: Client -> Bool
specialClient (clientName1 -> "kin") = True
specialClient _ = False

greet ::ClientR -> String
greet IndividualR { person = PersonR { firstName = f1 } } = "Hi " ++ f1
greet _ = "Who are you?"

greetWithPragma :: ClientR -> String 
greetWithPragma IndividualR { person = PersonR { firstName } } = "Hi " ++ firstName
greetWithPragma _ = "Who are you?"


greetWithPragma2 :: ClientR -> String 
greetWithPragma2 IndividualR { person = PersonR { .. } } = "Hi " ++ firstName
greetWithPragma2 _ = "Who are you?"

