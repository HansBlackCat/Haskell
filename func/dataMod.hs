module dataMod 
( Point(..), Shape(..), baseCircle, area, nudge) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point 
    deriving (Show)

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0) 

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape 
nudge (Circle (Point x1 y1) r) delx dely = Circle (Point (x1+delx) (y1+dely)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) delx dely 
    = Rectangle (Point (x1+delx) (y1+dely)) (Point (x2+delx) (y2+dely))

    
