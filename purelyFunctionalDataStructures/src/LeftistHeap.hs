module LeftistHeap (module Heap, LeftistHeap) where
  import Heap 
  
  data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)
  
  rank E = 0
  rank (T r _ _ _) = r 
  
  makeT x a b = if rank a >= rank b 
                  then T (rank b + 1) x a b 
                  else T (rank a + 1) x b a
  
  instance Heap LeftistHeap where 
    h_empty = E 
    h_isEmpty E = True 
    h_isEmpty _ = False
    
    h_insert x = h_merge (T 1 x E E)  
    h_merge h E = h
    h_merge E h = h 
    h_merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
      if x <= y 
        then makeT x a1 (h_merge b1 h2)
        else makeT y a2 (h_merge h1 b2) 
    h_findMin E = error "LeftistHeap.findMin: Empty Heap"
    h_findMin (T _ x a b) = x 
    h_deleteMin E = error "LeftistHeap.deleteMin: Empty Heap"
    h_deleteMin (T _ x a b) = h_merge a b 
