--Solve the previous exercise using pairs rather than Points.

inc :: (Int, Int) -> (Int, Int)
inc (x, y) = (x +1, y + 1)

p :: (Int, Int)
p = (-1, 4)

main = print $ inc p
