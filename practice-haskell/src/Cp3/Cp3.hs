{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}

module Cp3.Cp3 where

import Cp2.Cp2
import Data.List
import Data.Function
import GHC.Exts

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

filterNot f = filter (not. f) 

filterGovOrgs = filter isGovOrgs
  where 
    isGovOrgs = 
      (\case GovOrg a -> True
             _ -> False)

-- uncurry f = \(x, y) -> f x y
-- curry f = \x y -> f (x, y)
-- flip f = \x y -> f y x

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

data InfNum a = MinusInfinity
              | Number a 
              | PlusInfinity 
              deriving Show 

ninfMax MinusInfinity x = x
ninfMax x MinusInfinity = x
ninfMax PlusInfinity _ = PlusInfinity
ninfMax _ PlusInfinity = PlusInfinity
ninfMax (Number a) (Number b) = Number (max a b)

infMax MinusInfinity (Number a) = Number a
infMax (Number a) MinusInfinity = Number a
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

-- filter vs filterNot => partition
-- find
-- takeWhile vs dropWhile => span

isIndividual :: Client -> Bool 
isIndividual (Individual {}) = True 
isIndividual _               = False

isCompanyR :: ClientR -> Bool 
isCompanyR (CompanyR {})   = True 
isCompanyR _               = False

checkAnalytics :: [Client] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

-- nubBy (O(n^2))
-- nubBy (==) [1,1,2,3,4] == [1,2,3,4]
-- nub == nubBy (==) // Remove duplicated -- but slow
-- union(By), intersect(By), insert(By)
-- `U`      , `n`          , insert

elem1' :: (Traversable t, Eq a) => a -> t a -> Bool 
elem1' a list 
  | find (== a) list == Just a  = True
  | find (== a) list == Nothing = False

compareClientR :: ClientR -> ClientR -> Ordering
compareClientR (IndividualR {person = p1}) (IndividualR {person = p2})
                                  = compare (firstName p1) (firstName p2)
compareClientR (IndividualR {}) _ = GT 
compareClientR _ (IndividualR {}) = LT 
compareClientR c1 c2              = compare (clientRName c1) (clientRName c2)

listOfClients
  = [ IndividualR (PersonR "H. G." "Wells")
    , GovOrgR "NTTF"  -- National Time Travel Foundation
    , CompanyR "Wormhole Inc." 3 (PersonR "Karl" "Schwarzschild") "Physicist"
    , CompanyR "Wormhole Inc." 10 (PersonR "John" "Neumann") "Z-Mathematician"
    , IndividualR (PersonR "Doctor" "")
    , IndividualR (PersonR "Sarah" "Jane")
    ]

-- sortBy compareClientR  listOfClients 
-- compare

companyDutiesAnalytics :: [ClientR] -> [String]
companyDutiesAnalytics = map (duty. head). 
  sortBy (\x y -> compare (length y) (length x)).
  groupBy (\x y -> duty x == duty y).
  filter isCompanyR

companyDutiesAnalytics' :: [ClientR] -> [String]
companyDutiesAnalytics' = map (duty. head). 
  sortBy (flip (compare `on` length)).
  groupBy ((==) `on` duty).
  filter isCompanyR

withPosition :: [a] -> [(Int, a)]
withPosition list = zip [0..length list] list 

duplicateOdd :: [Integer] -> [Integer]
duplicateOdd list = map (*2) $ filter odd list

duplicateOddList :: [Integer] -> [Integer]
duplicateOddList list = [2*x | x <- list, odd x]

-- {-# LANGUAGE TransformListComp #-} 
-- [x*y | x <- [1..4], y<- [3..9], then reverse]

-- import GHC.Exts
-- [x*y | x <- [1..4], y<- [3..9], then sortWith by x]

-- [(the p, m) | x<-[-1,1,2], y<-[1..4], let m = x*y, let p = m>0, then group by p using groupWith ]
-- > [(False,[-1,-2,-3,-4]),(True,[1,2,3,4,2,4,6,8])]

-- `then group by` e using `f` 
-- f :: (a -> b) -> [a] -> [[a]]
complicatedUsageOfApplicList = [(the p, m) | x<-[-1,1,2], y<-[1..4], let m = x*y, let p = m>0, then group by p using groupWith ]

alphaCal p
  | p > 10    = "A"
  | p > 5     = "B"
  | p > 0     = "C"
  | otherwise = "D"
calculateProp = [(the p, x) | x <- [-5..15], let p = alphaCal x, then group by p using groupWith ]
-- > [("A",[11,12,13,14,15]),("B",[6,7,8,9,10]),("C",[1,2,3,4,5]),("D",[-5,-4,-3,-2,-1,0])]

companyAnalytics :: [ClientR] -> [(String, [(PersonR, String)])]
companyAnalytics clients =
  [(the clientRName, zip person duty)
  | client@(CompanyR {..}) <- clients
  , then sortWith by duty 
  , then group by clientRName using groupWith
  , then sortWith by length client
  ]

-- {-# LANGUAGE ParallelListComp #-}
parListCompTest = [x*y | x <- [1,2,3] | y <- [1,2,3]]
-- > [1,4,9]

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x l -> f x : l) []

-- map f. map g == map (f. g)


-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- seeds: 1  ->  2  ->  3  ->  4
--        1>3?  2>3?    3>3?   4>3(O) ==> Stop 
--        [1,     2,     3]
-- Just (x, s) -> x: what append to list, s: new seed
enumUnfold :: Int -> Int -> [Int]
enumUnfold n m = unfoldr (\x -> if x > m 
                                  then Nothing
                                  else Just (x, x+1)) n

minSort :: [Integer] -> [Integer]
minSort = unfoldr (\case [] -> Nothing
                         xs -> Just (m, delete m xs)
                                where m = minimum xs)


