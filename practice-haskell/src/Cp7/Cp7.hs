{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Cp7.Cp7 where

import Data

import Control.Monad 
import Control.Lens
import Data.List
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)
import Data.Set (Set)
import qualified Data.Set as S

-- ------------------------------------------------------------------------
-- Apriori Algorithm
-- ------------------------------------------------------------------------
clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo = 
  (\case GovOrg c      -> S.fromList [InfoClientKind KindGovOrg]
         Company c p d -> S.fromList [InfoClientKind KindCompany, InfoClientDuty d]
         Individual p  -> S.fromList [InfoClientKind KindIndividual])

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

