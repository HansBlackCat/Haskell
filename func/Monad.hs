{-
 - Monad Rule
 - Left identity    return x >>= f == f x
 - Right identity   m >>= (\x -> return x) == m
 - Associative      (m >>= f) >>= g == m >>= (\x->f x >>= g)
 -}
import Control.Applicative
import Control.Monad
import Data.Monoid 
import Control.Monad.Writer

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


--More Monad--

isBigGang :: Int -> (Bool, String)
isBigGang x = (x>9, "Compare gang size to 9.")

applyLog :: (Monoid o) => (a, o) -> (a -> (b, o)) -> (b, o)
applyLog (x, log) f = 
    let (y, newLog) = f x 
    in (y, log `mappend` newLog)

type Food = String 
type Price = Sum Int 

addDrink :: Food -> (Food, Price) 
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whisky", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int 
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog :: Writer [String] Int 
multiWithLog = do 
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

gcd' :: Int -> Int -> Int 
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0 = do 
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do 
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)

gcdRev :: Int -> Int -> Writer [String] Int 
gcdRev a b
    | b == 0 = do 
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do 
        result <- gcdRev b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result 


newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where 

instance Monoid (DiffList a) where 
    mempty = DiffList (\xs-> []++xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs->f (g xs))

gcdRev' :: Int -> Int -> Writer (DiffList String) Int 
gcdRev' a b
    | b == 0 = do 
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do 
        result <- gcdRev' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result 
