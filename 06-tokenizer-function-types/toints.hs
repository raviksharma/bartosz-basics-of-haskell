--Implement function toInts that takes a number in the form of a string and returns a list of its digits as integers.

import Data.Char

toInts :: String -> [Int]
toInts (i : rest) = digitToInt i : toInts rest
toInts [] = []

main = print $ toInts "2013"
