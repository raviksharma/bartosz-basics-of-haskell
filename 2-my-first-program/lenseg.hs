pyth (a, b) = a * a + b * b
lenSeg ((x, y), (x', y')) = sqrt $ pyth (x' - x, y' - y)
main = print $ lenSeg ((1, 0.5), (-1, -0.5))
