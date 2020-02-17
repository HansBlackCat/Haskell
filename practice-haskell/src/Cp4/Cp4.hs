module Cp4.Cp4 where

import Cp2.Cp2 
import Cp3.Cp3 
import qualified Data.Map as M 
import qualified Data.Set as S

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
