-- Priority Queues, using heaps.
--
-- Based on code by L. Paulson in _ML for the Working Programmer_.
--
-- Adapted for Haskell by Melissa O'Neill.
-- with bugfix to leftrem by Bertram Felgenhauer.
--
-- Code may be used freely, as long as attributions remain accurate.  

module PriorityQ (PriorityQ, empty, isEmpty, minKeyValue, minKey, minValue,
                  insert, deleteMin, deleteMinAndInsert) where

data PriorityQ k v = Lf
                   | Br {-# UNPACK #-} !k v !(PriorityQ k v) !(PriorityQ k v)
               deriving (Eq, Ord, Read, Show)

empty :: PriorityQ k v
empty = Lf

isEmpty :: PriorityQ k v -> Bool
isEmpty Lf  = True
isEmpty _   = False

minKeyValue :: PriorityQ k v -> (k, v)
minKeyValue (Br k v _ _)    = (k,v)
minKeyValue _               = error "Empty heap!"

minKey :: PriorityQ k v -> k
minKey (Br k v _ _)         = k
minKey _                    = error "Empty heap!"

minValue :: PriorityQ k v -> v
minValue (Br k v _ _)       = v
minValue _                  = error "Empty heap!"

insert :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
insert wk wv (Br vk vv t1 t2)
               | wk <= vk   = Br wk wv (insert vk vv t2) t1
               | otherwise  = Br vk vv (insert wk wv t2) t1
insert wk wv Lf             = Br wk wv Lf Lf

siftdown :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v -> PriorityQ k v
siftdown wk wv Lf _             = Br wk wv Lf Lf
siftdown wk wv (t @ (Br vk vv _ _)) Lf 
    | wk <= vk                  = Br wk wv t Lf
    | otherwise                 = Br vk vv (Br wk wv Lf Lf) Lf
siftdown wk wv (t1 @ (Br vk1 vv1 p1 q1)) (t2 @ (Br vk2 vv2 p2 q2))
    | wk <= vk1 && wk <= vk2    = Br wk wv t1 t2
    | vk1 <= vk2                = Br vk1 vv1 (siftdown wk wv p1 q1) t2
    | otherwise                 = Br vk2 vv2 t1 (siftdown wk wv p2 q2) 

deleteMinAndInsert :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
deleteMinAndInsert wk wv Lf             = error "Empty PriorityQ"
deleteMinAndInsert wk wv (Br _ _ t1 t2) = siftdown wk wv t1 t2

leftrem :: PriorityQ k v -> (k, v, PriorityQ k v)
leftrem (Br vk vv Lf Lf) = (vk, vv, Lf)
leftrem (Br vk vv t1 t2) = (wk, wv, Br vk vv t2 t) where
    (wk, wv, t) = leftrem t1
leftrem _                = error "Empty heap!"

deleteMin :: Ord k => PriorityQ k v -> PriorityQ k v
deleteMin (Br vk vv Lf _) = Lf
deleteMin (Br vk vv t1 t2) = siftdown wk wv t2 t where
    (wk,wv,t) = leftrem t1
deleteMin _ = error "Empty heap!"

-- The size function isn't use by the current code, but an implementation
-- was suggested by Bertram Felgenhauer on Haskell Cafe, so it is included
-- here.

-- Return number of elements in the priority queue. 
-- /O(log(n)^2)/ 
size :: PriorityQ k v -> Int 
size Lf = 0 
size (Br _ _ t1 t2) = 2*n + rest n t1 t2 where 
    n = size t2 
    -- rest n p q, where n = size q, and size p - size q = 0 or 1 
    -- returns 1 + size p - size q. 
    rest :: Int -> PriorityQ k v -> PriorityQ k v -> Int 
    rest 0 Lf _ = 1 
    rest 0 _  _ = 2 
    rest n (Br _ _ p1 p2) (Br _ _ q1 q2) = case r of 
        0 -> rest d p1 q1 -- subtree sizes: (d or d+1), d; d, d 
        1 -> rest d p2 q2 -- subtree sizes: d+1, (d or d+1); d+1, d 
      where (d, r) = (n-1) `quotRem` 2 
