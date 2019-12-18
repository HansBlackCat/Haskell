applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing 
applyMaybe (Just x) f = f x

type Birds = Int
type Pole  = (Birds, Birds)

landDefault :: Maybe Pole 
landDefault = Just (0,0)

landLeft :: Birds -> Pole -> Maybe Pole 
landLeft n (left, right) -- = (left + n, right)
    | abs ((left+n) - right) < 4 = Just (left+n, right)
    | otherwise                  = Nothing

landRight :: Birds -> Pole -> Maybe Pole 
landRight n (left, right) -- = (left, right + n)
    | abs (left - (right+n)) < 4 = Just (left, right+n)
    | otherwise                  = Nothing

banana :: Pole -> Maybe Pole 
banana _ = Nothing

x -: f = f x

{-
 - foo :: Maybe String
 - foo = Just 3   >>= (\x ->
 -       Just "!" >>= (\y ->
 -       Just (show x ++ y)))
 -}

foo :: Maybe String
foo = do 
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y) 

-- landDefault >>= landLeft 2 >>= landRight 2 >>= landLeft 1
routine :: Maybe Pole 
routine = do 
    start <- return (0,0)
    first <- landLeft 2 start 
    second <- landRight 2 first 
    landLeft 1 second 

justH :: Maybe Char
justH = do 
    (x:xs) <- Just "Hello"
    return x

failMatching :: Maybe Char 
failMatching = do 
    (x:xs) <- Just ""
    return x 

listOfTuples :: [(Int, Char)]
listOfTuples = do 
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)

-- [x|x<-[1..50], '7' `elem` show x]
-- [1..50] >>= (\x->guard ('7' `elem` show x) >> return x)
sevenOnly :: [Int]
sevenOnly = do 
    x<-[1..50]
    guard ('7' `elem` show x)
    return x


