{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Cp6.Cp6 where

import Data.List
import Data.Map as M hiding (foldr, map)
import Control.Lens
import Data.Char

-- Data Mining Algorithms
-- cluster algorithms

-- ------------------------------------------------------------------------
-- K-means
-- ------------------------------------------------------------------------
-- cluster ~= sqrt (n/2) -- n: Data num

class Ord v => Vector v where 
  distance :: v -> v -> Double
  centroid :: [v] -> v

-- {-# LANGUAGE FlexibleInstances #-}
-- Enable Vector (Double, Double) 
instance Vector (Double, Double) where 
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = 
    let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst
        n = fromIntegral $ length lst
      in (u/n, v/n)

-- {-# LANGUAGE MultiParamTypeClass #-}
class Vector v => Vectorizable e v where
  toVector :: e -> v 

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id -- (Double, Double) -> (Double, Double)

kMeans :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initializer (function)
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold const
  -> [v] -- centroids after convergence (Output)
kMeans i k points = kMeansBase (i k points) points 

kMeansBase :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeansBase centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points 
      oldNewCentroids = newCentroidPhase assignments
      -- (`Old`, `New`)
      newCentroids = map snd oldNewCentroids
    in 
      if thresholdChecking oldNewCentroids threshold
        then newCentroids 
        else kMeansBase newCentroids points threshold

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                        in M.adjust (p:) chosenC m)
              initialMap points
  where compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

-- threshold checking
thresholdChecking :: (Vector v) => [(v,v)] -> Double -> Bool 
thresholdChecking centroids threshold =
  foldr (\(x,y) s -> s + distance x y) 0.0 centroids<threshold

-- Support Function
simpleClusterNum n = round $ sqrt (n/2)
-- End Support Function

-- kMeans DEBUG
kfuncDEBUG :: Int -> [e] -> [(Double,Double)]
kfuncDEBUG 0 _ = []
kfuncDEBUG n v = (fromIntegral n, fromIntegral n) : kfuncDEBUG (n-1) v
kinfo = [(1,1),(1,2),(4,4),(4,5)] :: [(Double,Double)]
-- kMeans kfuncDEBUG 2 kinfo 0.001
-- END kMeans DEBUG

-- ------------------------------------------------------------------------
-- lens
-- ------------------------------------------------------------------------
-- lens package: `lens`, `fclabels`, `data-accessor`, `data-lens`
-- `lens` is huge so if possible use `microlens`

data ClientL i = GovOrgL     i String
               | CompanyL    i String PersonL String
               | IndividualL i PersonL
               deriving (Show)
data PersonL = PersonL String String  deriving (Show)

-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- Build a Lens from a getter and a setter.
-- Lens': structure does not change when value change
firstNameL :: Lens' PersonL String 
firstNameL = lens (\(PersonL f _) -> f) (\(PersonL _ l) newF -> PersonL newF l)

lastNameL :: Lens' PersonL String 
lastNameL = lens (\(PersonL _ l) -> l) (\(PersonL f _) newL -> PersonL f newL)

fullNameL :: Lens' PersonL String 
fullNameL = lens (\(PersonL f l) -> f ++ " " ++ l) 
                 (\_ newFullName -> case words newFullName of 
                                      f:l:_ -> PersonL f l
                                      _     -> error "Incorrect Name")

-- {-# LANGUAGE LambdaCase #-}
{-
identifier :: Lens (ClientL i) (ClientL j) i j
identifier = lens (\case (GovOrgL i _)      -> i 
                         (CompanyL i _ _ _) -> i 
                         (IndividualL i _)  -> i)
                  (\client newId -> case client of 
                    GovOrgL _ n      -> GovOrgL newId n 
                    CompanyL _ n p r -> CompanyL newId n p r 
                    IndividualL _ p  -> IndividualL newId p)
-}

data ClientTL i = GovOrgTL { _identifier :: i, _name :: String }
                | CompanyTL { _identifier :: i, _name :: String
                            , _person :: PersonTL, _duty :: String }
                | IndividualTL { _identifier :: i, _person :: PersonTL }
                deriving (Show)
data PersonTL = PersonTL { _firstName :: String, _lastName :: String } deriving (Show)

-- {-# LANGUAGE TemplateHaskell #-}
makeLenses ''ClientTL
makeLenses ''PersonTL 
-- GHC Command Line Argument -ddump-splices

fullNameTL :: Lens' PersonTL String 
fullNameTL = lens (\(PersonTL f l) -> f ++ " " ++ l) 
                  (\_ newFullName -> case words newFullName of 
                                       f:l:_ -> PersonTL f l
                                       _     -> error "Incorrect Name")

-- LensTEST
lper = PersonTL "John" "Smith"
ltView = (view firstName lper, lper ^. lastName) -- ("John","Smith")

lcli = IndividualTL 3 (PersonTL "John" "Smith")
-- `view` & `^.`
ltView2 = (view (person . lastName) lcli, lcli ^. person . fullNameTL) -- ("Smith","John Smith")
-- `set` & `.~
ltSet = (set identifier 4 lcli, person.lastName .~ "Kox" $ lcli) -- (IndividualTL {_identifier = 4, _person = PersonTL {_firstName = "John", _lastName = "Smith"}},IndividualTL {_identifier = 3, _person = PersonTL {_firstName = "John", _lastName = "Kox"}})
-- `&` operator
ltSetF = lcli & person.fullNameTL .~ "Lee JB" -- IndividualTL {_identifier = 3, _person = PersonTL {_firstName = "Lee", _lastName = "JB"}}
-- operators
ltOp = lcli & identifier +~ 2 -- IndividualTL {_identifier = 5, _person = PersonTL {_firstName = "John", _lastName = "Smith"}}
-- `over` & `%~` operator
ltOp2 = lcli & over identifier (+2) -- IndividualTL {_identifier = 5, _person = PersonTL {_firstName = "John", _lastName = "Smith"}}
ltOp3 = lcli & person.firstName %~ (++ "ee") -- IndividualTL {_identifier = 3, _person = PersonTL {_firstName = "Johnee", _lastName = "Smith"}}
ltOp4 = lcli & person.fullNameTL %~ (map toUpper) -- IndividualTL {_identifier = 3, _person = PersonTL {_firstName = "JOHN", _lastName = "SMITH"}}

-- lens in tuple
ltup = ("e", 4) & set _2 6 -- ("e", 6)
ltup2 = ("e", 4) & _2 %~ (*20) -- ("e", 80)

-- Maybe lens `^?`, `^?!`
lls = "abc" ^? _head -- Just 'a'
lls2 = "abc" ^?! _tail -- "bc"
lls3 = "abc" & (_head .~ 'd') -- "dbc"
lls4 = "abc" & (_tail %~ map toUpper) -- "aBC"

lls5 = (1,2,3) ^?! _3 -- e
-- not List
-- lls6 = [1,2,3] ^?! _2
llsTemp = "abcdefghijklm"
lls6 = (llsTemp ^? _init, llsTemp ^? _last) -- (Just "abcdefghijkl",Just 'm')
lls7 = llsTemp & (_last %~ toUpper) -- "abcdefghijklM"

llsPep = [PersonTL "Jack" "Smith", PersonTL "Mary" "B."]
lls8 = llsPep & traversed.firstName %~ map toUpper -- [PersonTL {_firstName = "JACK", _lastName = "Smith"},PersonTL {_firstName = "MARY", _lastName = "B."}]
-- END LensTEST

-- ------------------------------------------------------------------------
-- K-Means 2
-- ------------------------------------------------------------------------

data KMeansState e v = 
  KMeansState { _centroids :: [v]
              , _points :: [e]
              , _err :: Double
              , _threshold :: Double 
              , _steps :: Int}
              deriving (Show)
makeLenses ''KMeansState

-- 1.0/0.0 == Infinity
initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v 
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

clusterAssignmentPhaseS :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhaseS = undefined

kMeansS :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansS i n pts t = view centroids $ kMeansSBase (initializeState i n pts t)

kMeansSBase :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeansSBase state = 
  let assignments = clusterAssignmentPhaseS state
      state1      = state & centroids.traversed
                          %~ (\c -> centroid $ fmap toVector
                                             $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
    in if state3 ^. err < state3 ^. threshold 
         then state3
         else kMeansSBase state3
