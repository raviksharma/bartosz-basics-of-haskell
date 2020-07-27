-- Use foldl to calculate the sum of squares given a list of doubles.

accf :: Int -> Int -> Int
accf a b = a + b * b

main = print $  foldl accf 0 [3, 4, 5]
