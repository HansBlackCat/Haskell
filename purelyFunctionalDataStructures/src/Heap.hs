module Heap(Heap(..)) where
  class Heap h where
    h_empty :: Ord a => h a 
    h_isEmpty :: Ord a => h a -> Bool
    
    h_insert :: Ord a => a -> h a -> h a 
    h_merge :: Ord a => h a -> h a -> h a 
    h_findMin :: Ord a => h a -> a 
    h_deleteMin :: Ord a => h a -> h a 
 

