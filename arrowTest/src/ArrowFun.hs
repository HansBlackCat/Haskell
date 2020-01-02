module ArrowFun (
    module Control.Arrow,
    module Control.Category,
    module Prelude,
    module ArrowFun
  )where

  import Control.Arrow
  import Control.Category
  import Prelude hiding (id,(.))


  newtype SimpleFunc a b = SimpleFunc { runF :: a -> b }

  instance Arrow SimpleFunc where
    arr = SimpleFunc
    first (SimpleFunc f) = SimpleFunc (mapFst f)
      where mapFst g (a, b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
      where mapSnd g (a, b) = (a, g b)
  instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g. f)
    id = arr id 
    
  split :: (Arrow a) => a b (b, b)
  split = arr (\x -> (x, x))
  unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d 
  unsplit = arr. uncurry  -- arr (\op (x, y) -> x `op` y)  




