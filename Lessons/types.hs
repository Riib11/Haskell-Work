module Shapes
( Point(..)
, Shape(..)
, surface
, translate
, baseCircle
, baseRectangle
) where

data Point = Point Float Float deriving(Show)
data Shape = Circle Point Float | Rectangle Point Point deriving(Show)

surface :: Shape -> Float
surface (Circle _ r) = pi*r^2
surface (Rectangle (Point x1 x2) (Point x3 x4)) = (abs$x1-x3) * (abs$x2-x4)

translate :: Shape -> Float -> Float -> Shape  
translate (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
translate (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Bounded, Eq, Ord, Show, Read)

type DayList = [Day] -- synonym

-- phoneBook :: [(String,String)]
-- phoneBook = [
--     ("henry","925-323-1487")
-- ]

-- OR

type Name = (String,String)
type PhoneNumber = (Int,Int,Int)
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook n pn pb = elem (n,pn) pb

phoneBook :: PhoneBook
phoneBook =
    [(("Henry","Blanchette"),(925,323,1487))
    ,(("Wilwob","Thewaddabob"),(123,456,789))
    ]

type AssocList k v = [(k,v)]