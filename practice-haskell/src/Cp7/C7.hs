module Cp7.C7 where

import           Data

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer    hiding (Product)
import           Data.List
import           Data.Set                (Set)
import qualified Data.Set                as S

import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.State
import           System.Random

-- monad transormers

tab1 = [1,2,3] >>= \x -> [2*x, 3*x]
-- [2,3,4,6,6,9]
tab1c = concat $ map (\x -> [2*x, 3*x]) [1,2,3]
-- [2,3,4,6,6,9]

tab2 = do
  x <- [1,2,3]
  y <- [7,8,9]
  return $ x*y
-- [7,8,9,14,16,18,21,24,27]

-- join :: Monad m => m (m a) -> m a
-- x >>= f == join $ fmap f x

tab3 = mapM (\x -> Just (2*x)) [1,2,3]
-- Just [2,4,6]
tab3a = mapM_ (\x -> Just (2*x)) [1,2,3]
-- Just ()

tab4 :: Integer -> [Integer]
tab4 n = [n-1, n+1]
tab4a n = [16, n^2]
tab4p = tab4 3 `mplus` tab4a 10
-- [2,4,16,100]

tab5 = do
  x <- [1..10]
  guard (x>6)
  return x
-- [7,8,9,10]

tab6 = msum [[1], [2,3], [4,6,8]]
-- [1,2,3,4,6,8]
tab6a = msum [Just 1, Nothing, Just 3, Nothing]
-- Just 1

{- 7-2
find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f l =
  let listJusted = map (\x -> Just x) l
-}

-- Writer Practice
data Entry = InEntry
    { idE :: Int
    , msg :: String
    }
    deriving (Show, Eq, Ord)

logMsg :: String -> Writer [Entry] ()
logMsg f = tell [InEntry 20 f]
-- λ> logMsg "as"
-- WriterT (Identity ((),[InEntry {id = 20, msg = "as"}]))
-- λ> runWriter $ logMsg "as"
-- ((),[InEntry {id = 20, msg = "as"}])

-- ---------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------
-- State 1
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

-- randomRIO :: Random a => (a, a) -> IO a
-- randomIO  :: Random a => IO a

-- randomR(1,6) :: StdGen -> (Int, StdGen)
rollDie :: State StdGen Int
rollDie = state $ randomR (1,6)

-- state :: (s -> (a, s)) -> m a
-- get   :: m s
-- put   :: s -> m ()

-- evalState :: State s a -> s -> a
-- runState  :: State s a -> s -> (a,s)
{-
rollDie' :: State StdGen Int
rollDie' = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value
-}

rollDice :: State StdGen (Int, Int)
rollDice = liftA2 (,) rollDie rollDie

-- random :: (Random a , RandomGen g) => g -> (a, g)
getRandom :: Random a =>State StdGen a
getRandom = state random

allRandoms :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allRandoms = (,,,,,,) <$> getRandom
                      <*> getRandom
                      <*> getRandom
                      <*> getRandom
                      <*> getRandom
                      <*> getRandom
                      <*> getRandom
-- evalState allRandoms (mkStdGen 43)

-- State 2
-- https://wiki.haskell.org/State_Monad

-- runstate (return 'X') 1 == ('X',1)
{-
return 'X' --with init 1--            :: State Int Char
runState (return 'X') --with init 1-- :: Int -> (Char, Int)
init, final state == 1                :: Int
final value == 'X'                    :: Char
-}

-- runState get 1 == (1,1)
{-
get                                   :: State Int Int
runState get                          :: Int -> (Int, Int)
init, final state == 1                :: Int
final value == 1                      :: Int
-}

-- runState (put 5) 1 == ((),5)
{-
put 5                                 :: State Int ()
runState (put 5)                      :: Int -> ((), Int)
init state == 1                       :: Int
final state == 5                      :: Int
final value == ()                     :: ()
-}

-- runState (do {put 5; return 'X'}) 1 == ('X',5)
{-
do {put 5; return 'X'}                :: State Int Char
runState (do {put 5; return 'X'})     :: Int -> (Char, Int)
init state == 1                       :: Int
-}

-- runState (do {put "Hola"; return 1}) "Hello" == (1,"Hola")

-- runState (do {x <- get; put (x+1); return x}) 5 == (5,6)

-- runState (do {x <- get; put (x-1); get}) 1 == (0,0)

-- runState (modify (+10)) 1 == ((),11)
-- runState (gets (+5)) 1 == (6,1)
-- evalState (gets (+5)) 1 == 6
-- execState (gets (+5)) 1 == 1

{-
x = 3
x = x+1
print(x)

print((x+1))
print: p, (+1): g, allocate func: f
p.g.f 1
-}

{- (result, state)
(return 5) 1 => (5,1)
(put 5) 1    => ((),5)
(get 5)      => (5,5)
-}

-- runState (do {x<-get;put (x+10);get;modify (*10);return 0}) 5 == (0,150)
-- (5,5) -> ((),15) -> (15,15) -> ((),150) -> (0,150)

type GameValue = Int
type GameState = (Bool, Int)

-- State State@(Bool,Int) Value@(Int) ~ runState::(Int,(Bool,Int))
playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score

playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

startState :: (Bool, Int)
startState = (False, 0)

mainGame :: IO ()
mainGame = print $ evalState (playGame "abcaaacbbcabbab") startState

type MyState = Int
type MyStateMonad = State MyState

valFromState :: MyState -> Int
valFromState s = -s
nextState :: MyState->MyState
nextState x = 1+x

getNext :: MyStateMonad Int
getNext = state (\s -> let s'=nextState s in (valFromState s', s'))

inc3Sugared ::MyStateMonad Int
inc3Sugared = getNext >> getNext >> getNext

inc3Sugared' ::MyStateMonad Int
inc3Sugared' = do { getNext;getNext;getNext }

-- evalState inc3Sugared  0 == -3
-- evalState inc3Sugared' 0 == -3

data MyStateType = MST Int Bool Char Int
    deriving Show
getStateAny :: (Random a) => State StdGen a
getStateAny = do g <- get
                 (x,g') <- return $ random g
                 put g'
                 return x
getStateOne :: (Random a) => (a,a) -> State StdGen a
getStateOne bounds = do g <- get
                        (x,g') <- return $ randomR bounds g
                        put g'
                        return x
makeRandomValueST :: StdGen -> (MyStateType, StdGen)
makeRandomValueST = runState (do n <- getStateOne (1,100)
                                 b <- getStateAny
                                 c <- getStateOne ('a','z')
                                 m <- getStateOne (-n,n)
                                 return (MST n b c m))

-- State 3
threeCoins :: StdGen -> (Bool,Bool,Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

-- Making stack
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop [] = undefined
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), newStack1) = push 3 stack
      (_, newStack2) = pop newStack1
  in pop newStack2

-- newtype State s a = State { runState :: s -> (a, s) }
popS :: State Stack Int
popS = state $ \(x:xs) -> (x,xs)
-- runState (popS) [1,2,3] == (1,[2,3])

pushS :: Int -> State Stack ()
pushS a = state $ \xs -> ((), a:xs)
-- runState (pushS 10) [1,2,3] == ((),[10,1,2,3])

stackManipS :: State Stack Int
stackManipS = do pushS 3
                 _ <- popS
                 _ <- popS
                 popS
-- runState stackManipS [5,8,7,9] == (8,[7,9])

stackManipS' :: State Stack Int
stackManipS' = do pushS 3
                  popS
                  popS
                  popS
-- runState stackManipS' [5,8,7,9] == (8,[7,9])

stackStuffS :: State Stack ()
stackStuffS = do a <- popS
                 if a == 5
                   then pushS 5
                   else do pushS 3
                           pushS 8
-- runState stackStuffS [4,5,6,7] == ((),[8,3,5,6,7])

moreStuffS :: State Stack ()
moreStuffS = do a <- stackManipS'
                if a == 100
                  then stackStuffS
                  else return ()
-- runState moreStuffS [4,5,6,7] == ((),[6,7])
-- runState moreStuffS $ fmap (*100) [4,1,3,6] ==((),[8,3,600])

whatisReturnS :: State Stack ()
whatisReturnS = do return ()
-- runState whatisReturnS [4,5,6,7] == ((),[4,5,6,7])

stackyStack :: State Stack ()
stackyStack = do stackNow <- get
                 if stackNow == [1,2,3]
                   then put [8,3,1]
                   else put [9,2,1]
-- runState stackyStack [1,2,3] == ((),[8,3,1])
-- runState stackyStack [1,3,5] == ((),[9,2,1])




-- ---------------------------------------------------------------------
-- Writer
-- ---------------------------------------------------------------------
-- (value, log)

data EntryW = LogW
    { countW :: Int
    , msgW   :: String
    }
    deriving Show
logMsgW :: String -> Writer [EntryW] ()
logMsgW s = tell [LogW 1 s]
-- runWriter $ logMsgW "ad" == ((),[LogW {countW = 1, msgW = "ad"}])

-- runWriter (return 3 :: Writer String Int)        == (3,"")
-- runWriter (return 3 :: Writer (Sum Int) Int)     == (3,Sum {getSum = 0})
-- runWriter (return 3 :: Writer (Product Int) Int) == (3,Product {getProduct = 1})
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog :: Writer [String] Int
multiWithLog = do a <- logNumber 3
                  b <- logNumber 5
                  return (a*b)
-- multiWithLog == WriterT (Identity (15,["Got number: 3","Got number: 5"]))
-- runWriter multiWithLog == (15,["Got number: 3","Got number: 5"])

multiWithLog' :: Writer [String] Int
multiWithLog' = do a <- logNumber 3
                   b <- logNumber 5
                   tell ["Gonna multiply these two"]
                   return (a*b)
-- runWriter multiWithLog'
--   == (15,["Got number: 3","Got number: 5", "Gonna multiply these two"])

-- Euclid Algorithm
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0    = a
  | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0    = do tell ["Finished with " ++ show a]
                   return a
  | otherwise = do tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
                   gcd'' b (a `mod` b)
-- runWriter (gcd'' 8 3)
--   == (1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])
-- mapM_ putStrLn $ snd $ ~

-- ---------------------------------------------------------------------
-- Reader
-- ---------------------------------------------------------------------

addStuff :: Int -> Int
addStuff = do a <- (+3)
              b <- (*3)
              return (a*b)
-- addStuff 10 == 390


-- ---------------------------------------------------------------------
-- Difference List
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Continuous Monad
-- ---------------------------------------------------------------------

