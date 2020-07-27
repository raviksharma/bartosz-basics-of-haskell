--Define a data type Point with one constructor Pt that takes two Doubles, corresponding to the x and y coordinates of a point. Write a function inc that takes a Point and returns a new  Point whose coordinates are one more than the original coordinates. Use pattern matching.

data Point = Pt Double Double 
    deriving Show

inc :: Point -> Point
inc (Pt x y) = Pt (x + 1) (y + 1)

p :: Point
p = Pt (-1) 3

main = print $ inc p
