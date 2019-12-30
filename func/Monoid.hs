import Data.List
import Data.Char
import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F
import BSTree

-- 1. Binary func
-- 2. arg, reture -> same Type
-- 3. have id
-- 4. associative 

-- 1. mempty `mappend` x = x
-- 2. x `mappend` mempty = x
-- 3. x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z -- associative  

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare' :: String -> String -> Ordering 
lengthCompare' x y =
    (length x `compare` length y) `mappend` (vowel x `compare` vowel y) `mappend` (x `compare` y)
    where vowel = length. filter (`elem` "aeiou")
    
instance F.Foldable Tree where 
    foldMap f EmptyTree = mempty 
    foldMap f (Node x l r) = F.foldMap f l `mappend` 
                             f x `mappend`
                             F.foldMap f r

