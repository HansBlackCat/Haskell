import Data.List
import Data.Char
import Control.Applicative

data IMaybe a = INothing | IJust Int a deriving (Show)

instance Functor IMaybe where
    fmap f INothing = INothing
    fmap f (IJust instable stable) = IJust (instable+1) (f stable)

--data ZipList a = ZipList {getZipList :: [a]}
newtype ZipList a = ZipList {getZipList :: [a]}
newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

data CoolBool = CoolBool {getCoolBool :: Bool}
newtype CoolBool' = CoolBool' {getCoolBool' :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

type IntList = [Int]


--make fmap (+3) (1,2) = (4,2)
newtype Pair b a = Pair {getPair :: (a,b)}
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a++b

myAction' :: IO String
myAction' = (++) <$> (getLine) <*> (getLine)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])


