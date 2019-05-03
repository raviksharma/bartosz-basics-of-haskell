-- Implement function sumDig that takes a number in the form of a string and calculates the sum of its digits.

import Data.Char

toInts :: String -> [Int]
toInts [] = []
toInts (c : cs) = digitToInt c : toInts cs

sumDig :: String -> Int
sumDig s = acc 0 (toInts s)

acc :: Int -> [Int] -> Int
acc x [] = x
acc x (i : rest) = acc (x + i) rest 

main = print $ sumDig "30750"
