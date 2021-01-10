-- Use foldl to calculate the sum of squares given a list of doubles.

squares :: [Int] -> Int
squares = foldl (\acc x -> acc + x * x) 0

main = print $ squares [3, 4, 5]
