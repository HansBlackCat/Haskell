
module Data where

import Data.List
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Map as M

-- Clients
data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , person :: Person 
                      , duty :: String }
            | Individual { person :: Person }
            deriving (Eq, Show, Ord)

data Person = Person { firstName :: String
                     , lastName :: String 
                     , gender :: Gender }
            deriving (Eq, Show, Ord)

data Gender = Male | Female
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual
                deriving (Show, Eq, Ord)

-- Product
data Product = Product { productId :: Integer 
                       , productType :: ProductType }
             deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip
                 deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client 
                         , products :: [Product] }
              deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind           ClientKind
                  | InfoClientDuty           String 
                  | InfoClientGender         Gender 
                  | InfoPurchasedProduct     Integer 
                  | InfoPurchasedProductType ProductType 
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
                    deriving (Show, Eq, Ord)