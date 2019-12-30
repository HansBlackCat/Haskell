import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Monad.State

addStuff :: Int -> Int 
addStuff = do 
    a <- (*2)
    b <- (+10)
    return (a+b)

type Stack = [Int]

{-
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)
push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)
-}

stackMainip :: Stack -> (Int, Stack)
stackMainip stack = let 
    ((),newStack1) = push 3 stack 
    (a, newStack2) = pop newStack1
    in pop newStack2 

stackMainip' :: Stack -> (Int, Stack)
stackMainip' = do 
    push 3
    a <- pop 
    pop

{-
 - push 3 (list) >>= \_ ->
 - pop    (list) >>= \a ->
 - pop
 -}

statePop :: State Stack Int 
statePop = state $ \(x:xs) -> (x,xs) 
statePush :: Int -> State Stack ()
statePush a = state $ \xs -> ((),a:xs)

pop :: State Stack Int 
pop = do 
    (x:xs) <- get 
    put xs 
    return x

push :: Int -> State Stack ()
push x = do 
    xs <- get 
    put xs 


