{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
module Cp7.Cp7 where

import           Data

import           Control.Lens
import           Control.Monad
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Control.Monad.Writer hiding (Product)
import           Data.List
import           Data.Set             (Set)
import qualified Data.Set             as S

-- ------------------------------------------------------------------------
-- Apriori Algorithm
-- ------------------------------------------------------------------------
clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo =
  \case GovOrg _      -> S.fromList [InfoClientKind KindGovOrg]
        Company _ _ d -> S.fromList [InfoClientKind KindCompany, InfoClientDuty d]
        Individual _  -> S.fromList [InfoClientKind KindIndividual]

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $
                            S.insert (InfoPurchasedProductType t) pinfos)
  S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p

-- DEBUG
pTemp = [Product 3 TimeMachine, Product 10 TravelGuide, Product 11 Tool]
pTrans = productsToPurchaseInfo pTemp
-- fromList [InfoPurchasedProduct 3,InfoPurchasedProduct 10,InfoPurchasedProduct 11,InfoPurchasedProductType TimeMachine,InfoPurchasedProductType TravelGuide,InfoPurchasedProductType Tool]
cTrans = clientToPurchaseInfo (Company "1984 Inc." (Person "George" "Orwell" Male) "Director")
-- fromList [InfoClientKind KindCompany,InfoClientDuty "Director"]
-- END DEBUG



newtype FrequentSet = FrequentSet (Set PurchaseInfo)
                    deriving (Show, Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo)
    deriving (Eq, Ord)

instance Show AssocRule where
  show (AssocRule s1 s2) = show s1 ++ " => " ++ show s2



setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = sElts `S.isSubsetOf` tElts
      supp = length $ filter f trans
  in fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
  setSupport trans (FrequentSet $ a `S.union` b)
  / setSupport trans (FrequentSet a)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do Transaction t <- transactions
              e <- S.toList t
              let fs = FrequentSet $ S.singleton e
              guard $ setSupport transactions fs > minSupport
              return fs

-- Remove duplicates
-- Prelude.nub is slow
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b) == k-1
                        let fs = FrequentSet $ a `S.union` b
                        guard $ setSupport transactions fs > minSupport
                        return fs
  in Just (lk1, (k+1, lk1))

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets =
  do FrequentSet fs <- sets
     subset@(_:_) <- powerset $ S.toList fs
     let ssubset = S.fromList subset
         rule = AssocRule ssubset (fs `S.difference` ssubset)
     guard $ ruleConfidence transactions rule > minConfidence
     return rule

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions
  $ concat $ unfoldr (generateNextLk minSupport transactions) (1, generateL1 minSupport transactions)

-- ------------------------------------------------------------------------
-- Search Problem
-- ------------------------------------------------------------------------

-- Paths in a graph

-- (start, end) graph with no self-loops
-- Recommend Data.Graph
paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start:subpath
  in if start == end
        then return [end] `mplus` e_paths
        else e_paths

graph1 :: [(Int, Int)]
graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]
-- [[2013,501,2558],[2013,1004,2558]]

-- The Logic Monad

graph2 :: [(Int, Int)]
graph2 = [(2013,501),(501,2558),(501,1004),(1004,501),(2013,2558)]
-- take 3 $ paths graph2 2013 2558
-- [[2013,501,2558],[2013,501,1004,501,2558],[2013,501,1004,501,1004,501,2558]]

-- import Control.Monad.Logic
pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do (e_start, e_end) <- choices edges
                   guard $ e_start == start
                   subpath <- pathsL edges e_end end
                   return $ start:subpath
  in if start == end
        then return [end] `mplus` e_paths
        else e_paths
choices :: [a] -> Logic a
choices = msum . map return
-- observeMany 3 (pathsL graph2 2013 2558)
-- [[2013,501,2558],[2013,501,1004,501,2558],[2013,501,1004,501,1004,501,2558]]
-- Monad.Logic
-- [1,2] `interleave` [3,4] == [1,3,2,4]

-- Exercise 7-4
pathsFair :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsFair edges start end =
  let e_paths = edges >>= \(e_start, e_end) ->
                guard (e_start == start) >>
                pathsFair edges e_end end >>= \subpath ->
                return $ start:subpath
  in if start == end
        then return [end] `mplus` e_paths
        else e_paths

pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let e_paths = choices edges >>- \(e_start, e_end) ->
                guard (e_start == start) >>
                pathsLFair edges e_end end >>- \subpath ->
                return $ start:subpath
  in if start == end
        then return [end] `mplus` e_paths
        else e_paths

-- ------------------------------------------------------------------------
-- Monads and Lists Redux
-- ------------------------------------------------------------------------
addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

addPrefixF :: String -> Reader String String
addPrefixF s = do p <- ask
                  return $ p ++ s

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix
-- λ> runReader (addPrefixL ["one", "two"]) "**-"
-- ["**-one","**-two"]

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))
-- λ> runWriter $ logInformation ["One", "Two"]
-- ((), "One\nTwo\n")

logInformationR :: [String] -> Writer String ()
logInformationR infos = forM_ infos $ \s -> tell (s++"\n")

mySeqence :: (Traversable t, Monad m) => t (m a) -> m (t a)
mySeqence s = undefined

myMapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
myMapM f = sequenceA . fmap f

myMapM' :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
myMapM' = traverse

-- foldl ::            (a -> b ->   a) -> a -> t b ->   a
-- foldM :: Monad m => (a -> b -> m a) -> a -> t b -> m a
-- foldr ::            (a -> b ->   b) -> b -> t a ->   b

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f*x)) 1 [1..n]
-- factorialSteps 10 == WriterT (Identity (3628800,Sum {getSum = 10}))
-- tell -> mappend Sum

factorialSteps' :: Integer -> Writer (Sum Integer) Integer
factorialSteps' n = foldM (\f x -> do tell (Sum 1)
                                      return (f*x)) 1 [1..n]

powerset' :: [a] -> [[a]]
powerset' = filterM $ const [False,True]
-- const a == (\_ -> a)
-- active by nondeterministic list monads

-- [1,2,3] >>= (\x -> [(2*x),(-x)]) == [2,-1,4,-2,6,-3]

-- liftM :: Monad m => (a -> b) -> m a -> m b

-- return compare        :: (Monad m, Ord a) => m (a -> a -> Ordering)
-- return compare `ap` x :: (Monad m, Ord a) => m (a -> Ordering)

-- ap  :: Monad m       => m (a -> b) -> m a -> m b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

-- {-# LANGUAGE MonadComprehensions #-}
compreTest :: Integer -> Maybe Integer
compreTest x = do a <- Just (x+1)
                  b <- Just (x-1)
                  c <- Just (2*x)
                  return (a*b*c)

compreTest' :: Integer -> Maybe Integer
compreTest' x =
  [ a*b*c
  | a <- Just (x+1)
  , b <- Just (x-1)
  , c <- Just (2*x)]

-- ------------------------------------------------------------------------
-- Combining Monads
-- ------------------------------------------------------------------------

pathsWriter :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end
  = map execWriter (pathWriterBase edges start end)

pathWriterBase :: [(Int,Int)] -> Int -> Int -> [Writer [Int] ()]
pathWriterBase edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- pathWriterBase edges e_end end
                   return $ do tell [start]
                               subpath
  in if start == end
        then tell [start]:e_paths
             else e_paths

-- Monad Transformers
pathsWriterT' :: [(Int,Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do (e_start, e_end) <- lift edges
                   guard $ e_start == start
                   tell [start]
                   pathsWriterT' edges e_end end
  in if start == end
        then tell [start] `mplus` e_paths
             else e_paths

pathsWriterT :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

-- combines Reader ans Writer
readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift . tell $ show x
                         return $ x+1
-- runWriter (runReaderT readerWriterExample 3) == (4,"3")

-- p.272

