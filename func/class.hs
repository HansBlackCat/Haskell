import BSTree
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red    = True 
    Yellow == Yellow = True
    Green == Green   = True
    _ == _           = False

instance Show TrafficLight where
    show Red    = "Red Light"
    show Yellow = "Yellow Light"
    show Green  = "Green Light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Integer  where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno = id
instance YesNo (Maybe a) where
    yesno (Just _ ) = True
    yesno Nothing = False
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True  
    
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesRes noRes =
    if yesno yesnoVal 
        then  yesRes
        else noRes
            

