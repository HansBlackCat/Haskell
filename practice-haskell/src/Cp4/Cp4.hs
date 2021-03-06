{-# LANGUAGE RecordWildCards #-}

module Cp4.Cp4 where

import Cp2.Cp2 
import Cp3.Cp3 
import qualified Data.Map as M 
import qualified Data.Set as S
import Data.Tree
import Data.Graph
import Data.Monoid

-- Hackage: containers
-- stack.yaml
-- extra-deps: package that don't included in stackage, ex) - kingkang - 0.2.1.0
-- git repository also available

-- $ hoogle '(a -> b) -> ([a] -> [b])'
-- Prelude map :: (a -> b) -> [a] -> [b]
-- Data.List map :: (a -> b) -> [a] -> [b]
-- ..etc

-- hidden package -> stack build and restart

-- << Maps >>
-- import qualified Data.Map as M 
{-
 - M.empty -> fromList []
 - M.singleton "hello" 3 -> fromList [("hello", 3)]
 - M.fromList [("hello", 1), ("bye", 2), ("hello", 3)] -> fromList [("bye",2),("hello",3)]
 -}

m1 = M.singleton "hello" 3
m2 = M.fromList [("hello",1),("bye",2),("hello",3)]
m3 = let m1' = M.singleton "hello" 3
         m2' = M.insert "bye" 2 m1'
         m3' = M.insert "hello" 5 m2'
         m4' = M.insertWith (+) "hello" 7 m3'
     in (m1',m2',m3',m4')

-- m2 = fromList [("bye",2),("hello",3)] 
-- M.member, lookup, findWithDefault
-- M.member "hello" m2 -> True
-- M.lookup "hello" m2 -> Just 3
-- M.findWithDefault 0 "hello" m2 -> 3
-- M.delete "hello" m2 -> fromList [("bye", 2)]
-- M.adjust (+7) "hello" m2 -> fromList [("bye",2),("hello",10)]
-- M.alter (\(Just a) -> Just (a*2)) "hello" m2 -> fromList [("bye",2),("hello",6)]
-- M.union, M.intersectionWith, M.differenceWith

m4 = let m1' = M.fromList [("hello",3),("bye",4)]
         m2' = M.fromList [("hello",5),("welcome",6)]
     in (m1' `M.union` m2', M.intersectionWith (-) m1' m2')

m5 = (M.map (*2) m2, M.foldr (+) 0 m2)

-- M.assocs, M.findMin(Max), M.deleteMin(Max), M.updateMin(Max)
-- M.updateMin (\ a -> Just ("X" ++ a)) (M.fromList [(5,"a"), (3,"b")]) -> fromList [(3,"Xb"),(5,"a")]


-- << Sets >>
-- import qualified Data.Set as S

-- s1 :: Data.Set.Internal.Set [Char]
s1 = S.insert "welcome" $ S.singleton "hello"
-- fromList ["hello","welcome"]
s2 = S.fromList ["hello", "bye", "hello"]
-- fromList ["bye","hello"]
s3 = S.toList $ S.fromList ["duplicate", "boom", "duplicate"]
-- ["boom","duplicate"]
s4 = let set1 = S.insert "welcome" $ S.singleton "hello"
         set2 = S.fromList ["hello","bye"]
     in ( set1 `S.intersection` set2
        , "welcome" `S.member` set1
        , S.map length set2 )
-- (fromList ["hello"],True,fromList [3,5])

-- << Tree >>
pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) 
  = let subtreesTraversed = concat $ map (preOrder f) subtrees 
    in f v : subtreesTraversed

-- flatten == preOrder show
-- flatten pictureTree -> [1,2,3,4,5,6]
-- levels pictureTree -> [[1],[2,6],[3,4,5]]
-- fmap (*2) pictureTree -> Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 6, subForest = []},Node {rootLabel = 8, subForest = []},Node {rootLabel = 10, subForest = []}]},Node {rootLabel = 12, subForest = []}]}
-- foldr (+) 0 pictureTree -> 21

-- (value, key, [key])
timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = 
 [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
 ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
 ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

-- One-way street
timeMachinePrecedence:: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
-- graphFromEdges
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineThings = 
 let (g, v, _) = timeMachinePrecedence
 -- topSort :: Graph -> [Vertex]
 in map (\x -> let (k, _, _) = v x in k) $ topSort g

{-
ex)
wood -> walls -> done
          ^         ^
        plastic -> wheels  ..etc
-}

-- Both-way & One-way Graph
timeMachineTravel :: Graph
-- buildG :: Bounds -> [Edge] -> Graph
timeMachineTravel = buildG (103,2013)
  [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
  ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]

accessiblePath = path timeMachineTravel 1302 917
-- True
reachablePath = reachable timeMachineTravel 1302
-- reachable Path when start in 1302
-- [1302,2013,1408,917,1993,1614]

-- scc :: Graph -> Forest Vertex
makeScc = filter (\(Node {subForest = s}) -> s /= []) $ scc timeMachineTravel

-- flattenSCC & stronglyConnComp
sccPrintable = map flattenSCC $ stronglyConnComp timeMachineGraph


-- Ad-Hoc PolyMorhism
-- Ord a => ...
-- ^~~ ad hoc polymorphism
-- type to be accompanied by some functions
class Nameable n where
  name :: n -> String

instance Nameable Int where 
  name a = "It's Int!"

instance Nameable (ClientR) where
  name IndividualR {person = PersonR {firstName = f, lastName = n}}
    = f ++ " " ++ n
  name c = clientNameR c

initial :: Nameable n => n -> Char
initial n = head $ name n

class Duty p where
  whatsYourDuty :: p -> String

instance Duty ClientR where
  whatsYourDuty CompanyR {..} = duty 

data Complex = C Double Double deriving (Show, Eq)
 
instance Num Complex where
  (+) (C a1 b1) (C a2 b2) = C (a1 + a2) (b1 + b2)
  (-) (C a1 b1) (C a2 b2) = C (a1 - a2) (b1 - b2)
  (*) (C a1 b1) (C a2 b2) = C (a1*a2 - b1*b2) (a1*b2 + a2*b1)
  negate (C a b) = C (negate a) (negate b)
  fromInteger n  = C (fromInteger n) 0 
  abs (C a b)    = C (sqrt $ a*a + b*b) 0
  signum c@(C a b) = let C n _ = abs c in C (a/n) (b/n)
{-
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
-}

data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , price :: Double}
                   deriving (Show, Eq, Ord)

-- Simple Binary Tree
data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1 | Leaf1 
  deriving (Show)

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 _ Leaf1         = Nothing 
treeFind1 t (Node1 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind1 t l 
                              GT -> treeFind1 t r 

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t Leaf1           = Node1 t Leaf1 Leaf1
treeInsert1 t n@(Node1 v l r) = case compare t v of
                                  EQ -> n 
                                  LT -> Node1 v (treeInsert1 t l) r 
                                  GT -> Node1 v l (treeInsert1 t r)

-- Polymorphic Binary Tree
data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving (Show)

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 _ Leaf2         = Nothing
treeFind2 t (Node2 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind2 t l 
                              GT -> treeFind2 t r 

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a 
treeInsert2 t Leaf2           = Node2 t Leaf2 Leaf2 
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                  EQ -> n
                                  LT -> Node2 v (treeInsert2 t l) r
                                  GT -> Node2 v l (treeInsert2 t r)

treeConcat2 :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeConcat2 Leaf2 Leaf2          = Leaf2 
treeConcat2 Leaf2 r              = r
treeConcat2 l Leaf2              = l
treeConcat2 l r@(Node2 rv rl rr) = 
  treeConcat2 (treeInsert2 rv l) (treeConcat2 rl rr) 

treeFromList2 :: Ord a => [a] -> BinaryTree2 a
treeFromList2 [] = Leaf2 
treeFromList2 (x:xs) = treeInsert2 x (treeFromList2 xs)

-- DEBUG
treeDEBUG1 = treeFromList2 $ reverse [5,3,7]
treeDEBUG2 = treeFromList2 $ reverse [4,2,6]

-- TravelGuide
-- Duplicate (Deriving Ord vs instanciation)
-- Wrapper of data
newtype TGByPrice = TGByPrice TravelGuide deriving Eq
instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) = 
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

-- Binary Trees with Monoidal Cache
data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3
treeInsert3 v c (Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> Node3 v2 c2 l r 
    LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
    GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)

-- import Data.Monoid
-- Binary Trees with Monoid
data BinaryTree4 v c = Node4 v c (BinaryTree4 v c) (BinaryTree4 v c)
                     | Leaf4
                     deriving (Show, Eq, Ord)

cached :: Monoid c => BinaryTree4 v c -> c 
cached (Node4 _ c _ _) = c 
cached Leaf4           = mempty                   

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree4 v c -> BinaryTree4 v c 
treeInsert4 v c Leaf4 = Node4 v c Leaf4 Leaf4
treeInsert4 v c (Node4 v2 c2 l r) =
  case compare v v2 of 
    EQ -> Node4 v2 c2 l r 
    LT -> let newLeft  = treeInsert4 v c l
              newCache = c2 <> cached newLeft <> cached r 
          in  Node4 v2 newCache newLeft r 
    GT -> let newRight = treeInsert4 v c r
              newCache = c2 <> cached l <> cached newRight
          in  Node4 v2 newCache l newRight 

newtype Min = Min Double deriving (Show)
instance Semigroup Min where
  Min x <> Min y = Min $ min x y 
instance Monoid Min where 
  mempty  = Min infinity where infinity = 1/10
  mappend = (<>) -- From semigroup

{-
data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , price :: Double}
                   deriving (Show, Eq, Ord)
-}

modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m = map (\tg -> tg {price = m * price tg})

modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
modifyTravelGuidePriceMap m = M.map (\tg -> tg {price = m * price tg})

modifyTravelGuidePriceTree :: Double -> Tree TravelGuide -> Tree TravelGuide
modifyTravelGuidePriceTree m = fmap (\tg -> tg {price = m * price tg})

-- Using Functor
modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg {price = m * price tg})

-- Char, Integer => kind *
-- Maybe         => kind * -> *
-- Maybe Integer => kind *
