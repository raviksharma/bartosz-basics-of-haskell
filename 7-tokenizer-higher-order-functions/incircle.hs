-- Implement function inCircle2 that takes a list of 2-D points and returns only those that fit inside the circle of radius 2.
type Point = (Double, Double)
inCircle2 :: [Point] -> [Point]
inCircle2 = filter (\(x, y) -> sqrt (x*x + y*y) <= 2.0)

main = print $ inCircle2 [(0, 0), (2, -2), (1, -1), (1.9, 0.1), (10, 1)]
