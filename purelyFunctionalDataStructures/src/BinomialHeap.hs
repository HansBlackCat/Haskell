module BinomialHeap (module Heap, BinomialHeap) where
  import Heap 
  
  -- Node/ rank/ root/ Tree
  data Tree a = Node Int a [Tree a]
  newtype BinomialHeap a = BH [Tree a]
  
  rank (Node r x c) = r 
  root (Node r x c) = x 
  
  link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2 
      then Node (r+1) x1 (t2:c1)
      else Node (r+1) x2 (t1:c2)
  insTree t [] = [t]
  insTree t ts@(t':ts') = 
    if rank t < rank t'
      then t:ts 
      else insTree (link t t') ts'
      
  mrg ts [] = ts 
  mrg [] ts = ts
  mrg ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1 : mrg ts1' ts2 
    | rank t2 < rank t1 = t2 : mrg ts1 ts2'
    | otherwise         = insTree (link t1 t2) (mrg ts1' ts2') 
  
  removeMinTree [] = error "BinomialHeap.removeMinTree: Empty Heap"
  removeMinTree [t] = (t, [])
  removeMinTree (t:ts) = 
    if root t < root t'
      then (t, ts) 
      else (t', t:ts')
        where (t', ts') = removeMinTree ts
  
  instance Heap BinomialHeap where 
    h_empty = BH []
    h_isEmpty (BH ts) = null ts 
    
    h_insert x (BH ts) = BH (insTree (Node 0 x []) ts)
    h_merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)
    
    h_findMin (BH ts) = root t 
      where (t,_) = removeMinTree ts 
    
    h_deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
      where (Node _ x ts1, ts2) = removeMinTree ts 
      
